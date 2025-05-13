package Validation.EntityValidation.Agent

import Validation.{Results, States}
import GenericDefinitions.{AgentEntity, ResourceEntity, ProcessingContext}
import Validation.States.ValidationState
import Validation.Results.ValidationResult
import cats.implicits.*
import Validation.Utils.ExtractUtils.{extractBehaviourMessage, logger}
import Validation.EntityValidation.Agent.StateMachineValidations.stateMachineVals
import Validation.Utils.ReflectionExtractUtils.{checkResourceAccess,checkChannelAccess,checkMessageAccess, extractSendChannelPairs}
import AgentValidationMessageTemplates.*
import Utilz.ConfigDb

object AgentValidations {

  // A type alias: function (AgentEntity, ValidationState) => ValidationResult
  type AgentValidation = (AgentEntity, ValidationState) => ValidationResult

  private def checkNameNotEmpty(agent: AgentEntity, state: ValidationState): ValidationResult = {
    if (agent.name.trim.nonEmpty) {
      ValidationResult.valid
    } else {
      ValidationResult.fromError(emptyAgentName)
    }
  }

  private def checkAgentEmpty(agent: AgentEntity, state: ValidationState): ValidationResult = {
    if (agent.getStates.isEmpty &&
      agent.getTransitions.isEmpty &&
      agent.getResources.isEmpty) {
      ValidationResult.fromError(emptyAgent.format(agent.name))
    } else {
      ValidationResult.valid
    }
  }

  private def checkAllMessagesHandled(agent: AgentEntity, state: ValidationState): ValidationResult = {
    val allIncomingMessages = state.structState.agentIncomingMessages(agent.name)

    val allHandledMessages = agent.getStates.flatMap(
      state => {
        val allStateHandles = state.behaviors.flatMap(
          behavior => {
            val triggers = behavior.triggerMsgs.map(_.name).toSet
            val actionMessages = extractBehaviourMessage(using ProcessingContext.current)(behavior, allIncomingMessages)
            triggers ++ actionMessages
          }
        )
        allStateHandles
      }
    )

    if ConfigDb.`DIALS.General.debugMode` then logger.info(s"Agent ${agent.name} has handled messages: ${allHandledMessages.mkString(", ")}")

    val unhandledMessages = allIncomingMessages.map(_.name) -- allHandledMessages

    if ConfigDb.`DIALS.General.debugMode` then logger.info(s"Agent ${agent.name} has unhandled messages: ${unhandledMessages.mkString(", ")}")

    if (unhandledMessages.isEmpty) {
      ValidationResult.valid
    } else {
      ValidationResult.fromError(unhandledMessagesString.format(agent.name,unhandledMessages.mkString(", ")))
    }

  }

  private def checkDuplicateNames(agent: AgentEntity, state: ValidationState): ValidationResult = {
    val agentName = agent.name
    if (state.nameState.agentNameToHashes(agentName).size > 1) {
      ValidationResult.fromError(duplicateAgentName.format(agentName))
    } else {
      ValidationResult.valid
    }
  }
  
  private def checkDuplicateResources(agent: AgentEntity, state: ValidationState): ValidationResult = {
    val agentName = agent.name
    val resourceNametoHash = state.nameState.resourceNameToHashes(agentName)
//    Hashmap resource name to set of hashes found if anything has more than one hash duplicate is detected

    val duplicates = resourceNametoHash.filter { case (_, hashes) => hashes.size > 1 }
    if (duplicates.nonEmpty) {
      ValidationResult.fromError(duplicateResourceNames.format(duplicates.keys.mkString(", ")))
    } else {
      ValidationResult.valid
    }
  }

  private def checkResourceScopeChecks(agent: AgentEntity, state: ValidationState): ValidationResult = {
    val resources = agent.getResources ++ ResourceEntity.getTopLevelResources
    
    val allFieldResources = resources.flatMap(_.collectAllFieldResources)
    val allResources = resources ++ allFieldResources
    val allResourceString = allResources.map(_.name).toSet
    val allStates = agent.getStates
//    for each state check all behaviours onActiveAction code and actualAction code
    val allBehaviors = allStates.flatMap(_.behaviors)
    val ActiveActionCode = allBehaviors.flatMap(_.onActiveActionsCode )
    val ActualActionCode = allBehaviors.flatMap(_.actualActionsCode)
    val onSwitchCode = allStates.flatMap(_.onSwitchCode)

    val allCode = ActiveActionCode ++ ActualActionCode ++ onSwitchCode

    val allResourceAccesses = allCode.flatMap(code => checkResourceAccess(code))

    val outOfScope = allResourceAccesses.filterNot(resource => allResourceString.contains(resource))

    if (outOfScope.isEmpty) {
      ValidationResult.valid
    } else {
      ValidationResult.fromError(outOfScopeResources.format(agent.name, outOfScope.mkString(", ")))
    }

  }

  private def checkValidChannelAccess(agent: AgentEntity, state: ValidationState): ValidationResult = {
    val allStates = agent.getStates
    val allBehaviors = allStates.flatMap(_.behaviors)
    val ActiveActionCode = allBehaviors.flatMap(_.onActiveActionsCode )
    val ActualActionCode = allBehaviors.flatMap(_.actualActionsCode)
    val onSwitchCode = allStates.flatMap(_.onSwitchCode)

    val allCode = ActiveActionCode ++ ActualActionCode ++ onSwitchCode

    val allChannelAccesses = allCode.flatMap(code => checkChannelAccess(code))

    val undefinedChannels = allChannelAccesses.filterNot(channel => state.nameState.channelNameToHashes.contains(channel))

    if (undefinedChannels.isEmpty) {
      ValidationResult.valid
    } else {
      ValidationResult.fromError(invalidChannelComm.format(agent.name, undefinedChannels.mkString(", ")))
    }
  }

  private def checkValidDispatchSend(agent: AgentEntity, state: ValidationState): ValidationResult = {
    val allStates = agent.getStates
    val allBehaviors = allStates.flatMap(_.behaviors)
    val ActiveActionCode = allBehaviors.flatMap(_.onActiveActionsCode )
    val ActualActionCode = allBehaviors.flatMap(_.actualActionsCode)
    val onSwitchCode = allStates.flatMap(_.onSwitchCode)

    val allCode = ActiveActionCode ++ ActualActionCode ++ onSwitchCode

    val allMessageChannelPairs= allCode.flatMap(code => extractSendChannelPairs(code))
//    return message name and channel name
    val invalidPairs = allMessageChannelPairs.filterNot { case (msg, chan) =>
      state.structState.channelMessages.get(chan).exists(_.contains(msg))
    }

    if (invalidPairs.isEmpty) {
      ValidationResult.valid
    } else {
      ValidationResult.fromError(invalidSendChannel.format(agent.name, invalidPairs.map(_._1).mkString(", "), invalidPairs.map(_._2).mkString(", ")))
    }
  }

  private def checkValidMessageAccess(agent: AgentEntity, state: ValidationState): ValidationResult = {
    val allStates = agent.getStates
    val allBehaviors = allStates.flatMap(_.behaviors)
    val ActiveActionCode = allBehaviors.flatMap(_.onActiveActionsCode )
    val ActualActionCode = allBehaviors.flatMap(_.actualActionsCode)
    val onSwitchCode = allStates.flatMap(_.onSwitchCode)

    val allCode = ActiveActionCode ++ ActualActionCode ++ onSwitchCode

    val allMessageAccesses = allCode.flatMap(code => checkMessageAccess(code))

    val undefinedMessages = allMessageAccesses.filterNot(message => state.nameState.messageNameToHashes.contains(message))

    if (undefinedMessages.isEmpty) {
      ValidationResult.valid
    } else {
      ValidationResult.fromError(invalidMessage.format(agent.name, undefinedMessages.mkString(", ")))
    }
  }

  /**
   * List of all Agent-level validations
   * (Note: checkCurrentState is currently included twice—remove duplicates if unintended).
   */
  private val allValidations: List[AgentValidation] = List(
    checkNameNotEmpty,
    checkDuplicateNames,
    checkAgentEmpty,
    checkAllMessagesHandled,
    checkResourceScopeChecks,
    checkDuplicateResources,
    checkValidChannelAccess,
    checkValidDispatchSend,
    checkValidMessageAccess,
  ) ++ stateMachineVals

  /**
   * Applies all agent validations and accumulates errors/warnings
   */
  def validate(agent: AgentEntity, state: ValidationState): ValidationResult = {
    allValidations
      .map(fn => fn(agent, state))  
      .combineAll                   
  }
}
