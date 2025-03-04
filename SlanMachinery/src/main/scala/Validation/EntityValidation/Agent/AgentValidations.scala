package Validation.EntityValidation.Agent

import Validation.{Results, States}
import GenericDefinitions.AgentEntity
import Validation.States.ValidationState
import Validation.Results.ValidationResult
import cats.implicits.*
import Validation.Utils.ExtractUtils.{extractBehaviourMessage, logger}
import Validation.EntityValidation.Agent.StateMachineValidations.stateMachineVals

object AgentValidations {

  // A type alias: function (AgentEntity, ValidationState) => ValidationResult
  type AgentValidation = (AgentEntity, ValidationState) => ValidationResult

  private def checkNameNotEmpty(agent: AgentEntity, state: ValidationState): ValidationResult = {
    if (agent.name.trim.nonEmpty) {
      ValidationResult.valid
    } else {
      ValidationResult.fromError("Agent name cannot be empty.")
    }
  }

  private def checkAgentEmpty(agent: AgentEntity, state: ValidationState): ValidationResult = {
    if (agent.getStates.isEmpty &&
      agent.getTransitions.isEmpty &&
      agent.getResources.isEmpty) {
      ValidationResult.fromError(s"Agent ${agent.name} is empty. Possibly not defined or removed.")
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
            val actionMessages = extractBehaviourMessage(behavior, allIncomingMessages)
            triggers ++ actionMessages
          }
        )
        allStateHandles
      }
    )

    logger.info(s"Agent ${agent.name} has handled messages: ${allHandledMessages.mkString(", ")}")

    val unhandledMessages = allIncomingMessages.map(_.name) -- allHandledMessages

    logger.info(s"Agent ${agent.name} has unhandled messages: ${unhandledMessages.mkString(", ")}")

    if (unhandledMessages.isEmpty) {
      ValidationResult.valid
    } else {
      ValidationResult.fromError(s"Agent ${agent.name} has unhandled messages: ${unhandledMessages.mkString(", ")}")
    }

  }

  private def checkDuplicateNames(agent: AgentEntity, state: ValidationState): ValidationResult = {
    val agentName = agent.name
    if (state.nameState.agentNameToHashes(agentName).size > 1) {
      ValidationResult.fromError(s"Duplicate agent name found: $agentName")
    } else {
      ValidationResult.valid
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
    checkAllMessagesHandled
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
