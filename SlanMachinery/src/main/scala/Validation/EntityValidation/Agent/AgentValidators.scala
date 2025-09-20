package Validation.EntityValidation.Agent

import Validation.Results.ValidationResult
import Validation.States.ValidationState
import GenericDefinitions.{AgentEntity, DialsEntity, StateEntity}
import Utilz.{ConfigDb, CreateLogger}
import cats.implicits.*
import Validation.DialsValidator
import Validation.Utils.ReflectionExtractUtils.extractSendChannelPairs

object AgentValidators {
  val logger = CreateLogger(classOf[DialsValidator[AgentEntity]])
  // AgentEntity processing and validation
  given DialsValidator[AgentEntity] with
    def processIR(agent: AgentEntity, state: ValidationState): ValidationState = {
      val agentHash = agent.hashCode().toString
      if (state.visitedEntities.contains(agentHash)) state
      else {
        if ConfigDb.`DIALS.General.debugMode` then logger.info(s"Processing IR for agent: ${agent.name}")

        val agentList = agent.name::state.smState.agents

//        add agent hash so we dont check this object again in the future
//        adds agent to the top of state in agentList this will be used by state entity processIR so states can be assigned to the agent
//        merging here is order specific because we have to maintain current agent at top!
        val updatedState = state |+| ValidationState.empty.copy(
          visitedEntities = state.visitedEntities + agentHash,
          nameState = state.nameState.copy(
            agentNameToHashes = state.nameState.agentNameToHashes.updated(
              agent.name, state.nameState.agentNameToHashes.getOrElse(agent.name, Set.empty) + agentHash
            )
          ),
          smState = state.smState.copy(
            agents = agentList
          )
        )

        val processed = agent.getStates.foldLeft(updatedState) { (accState, state) =>
          summon[DialsValidator[DialsEntity]].processIR(state, accState)
        }





//        proceed to adding the state transitions to the mapping in struct state


        val resourceValidations = agent.getResources.foldLeft(processed) { (accState, resource) =>
          summon[DialsValidator[DialsEntity]].processIR(resource, accState)
        }


        val allStates = agent.getStates
        val allBehaviors = allStates.flatMap(_.behaviors)
        val ActiveActionCode = allBehaviors.flatMap(_.onActiveActionsCode)
        val ActualActionCode = allBehaviors.flatMap(_.actualActionsCode)
        val onSwitchCode = allStates.flatMap(_.onSwitchCode)

        val allCode = ActiveActionCode ++ ActualActionCode ++ onSwitchCode

        val allMessageChannelPairs = allCode.flatMap(code => extractSendChannelPairs(code))
//        convert to channel name : string to set [string] to be used in the state machine
        val allMessageChannelPairsMap = allMessageChannelPairs.groupBy(_._2).map { case (k, v) => (k, v.map(_._1).toSet) }


        val finalState = resourceValidations.copy(
          structState = resourceValidations.structState.copy(
            channelSentMessages = resourceValidations.structState.channelSentMessages |+| allMessageChannelPairsMap
          )
        )
        finalState

      }
    }

    def validate(agent: AgentEntity, state: ValidationState, result: ValidationResult): ValidationResult = {
      val agentHash = agent.hashCode().toString
      if (result.visitedEntities.contains(agentHash)) result
      else {
        if ConfigDb.`DIALS.General.debugMode` then logger.info(s"Validating agent: ${agent.name}")
        val agentResult = AgentValidations.validate(agent, state)
        val validatedAgents = result.copy(visitedEntities = result.visitedEntities :+ agentHash) |+| agentResult
        val processedStates = agent.getStates.foldLeft(validatedAgents) { (accState, st) =>
          summon[DialsValidator[DialsEntity]].validate(st, state, validatedAgents)
        }

        processedStates
      }
    }

}
