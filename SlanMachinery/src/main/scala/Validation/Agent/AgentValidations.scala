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

  private def checkCurrentState(agent: AgentEntity, state: ValidationState): ValidationResult = {
    // Just an example; adjust the message/condition as needed.
    if (agent.getCurrentState.isEmpty) {
      ValidationResult.valid
    } else {
      ValidationResult.fromWarning("No current state in the agent.")
    }
  }

  private def checkDuplicateNames(agent: AgentEntity, state: ValidationState): ValidationResult = {
    val agentName = agent.name
    if (state.agentNameToHashes(agentName).size > 1) {
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
    checkDuplicateNames
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
