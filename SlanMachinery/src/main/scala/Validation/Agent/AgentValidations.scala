package Validation.Agent

import Validation.{ValidationResult, ValidationState}
import GenericDefinitions.AgentEntity
import cats.implicits._  // for combineAll

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

  private def checkCurrentState(agent: AgentEntity, state: ValidationState): ValidationResult = {
    // Just an example; adjust the message/condition as needed.
    if (agent.getCurrentState.isEmpty) {
      ValidationResult.valid
    } else {
      ValidationResult.fromWarning(s"No current state in the agent ${agent.name}")
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
    checkCurrentState,
    checkDuplicateNames,
    checkAgentEmpty
  )

  /**
   * Applies all agent validations and accumulates errors/warnings
   */
  def validate(agent: AgentEntity, state: ValidationState): ValidationResult = {
    allValidations
      .map(fn => fn(agent, state))  
      .combineAll                   
  }
}
