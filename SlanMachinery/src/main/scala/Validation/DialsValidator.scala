package Validation

import GenericDefinitions.*
import Utilz.CreateLogger
import Validation.States.ValidationState
import Validation.Results.ValidationResult

import Validation.EntityValidation.Agent.AgentValidators.given
import Validation.EntityValidation.Model.ModelValidators.given
import Validation.EntityValidation.Channel.ChannelValidators.given
import Validation.EntityValidation.EntityInstance.EntityInstanceValidators.given
import Validation.EntityValidation.Groups.GroupValidators.given
import Validation.EntityValidation.State.StateValidators.given


// Define the type class for validation
trait DialsValidator[T] {
  def processIR(entity: T, state: ValidationState): ValidationState
  def validate(entity: T, state: ValidationState, result: ValidationResult): ValidationResult
}

// Define instances for each entity type
object DialsValidator {
  val logger = CreateLogger(classOf[DialsValidator[DialsEntity]])
  // Generic case for DialsEntity
  given DialsValidator[DialsEntity] with
    def processIR(entity: DialsEntity, state: ValidationState): ValidationState = entity match {
      case agent: AgentEntity         => summon[DialsValidator[AgentEntity]].processIR(agent, state)
      case model: ModelEntity         => summon[DialsValidator[ModelEntity]].processIR(model, state)
      case channel: ChannelEntity     => summon[DialsValidator[ChannelEntity]].processIR(channel, state)
      case alias: EntityInstanceAlias => summon[DialsValidator[EntityInstanceAlias]].processIR(alias, state)
      case group: GroupEntity         => summon[DialsValidator[GroupEntity]].processIR(group, state)
      case st: StateEntity         => summon[DialsValidator[StateEntity]].processIR(st, state)
      case _ =>
        logger.error(s"Unknown DialsEntity type: $entity")
        state
    }

    def validate(entity: DialsEntity, state: ValidationState, result: ValidationResult): ValidationResult = entity match {
      case agent: AgentEntity         => summon[DialsValidator[AgentEntity]].validate(agent, state, result)
      case model: ModelEntity         => summon[DialsValidator[ModelEntity]].validate(model, state, result)
      case channel: ChannelEntity     => summon[DialsValidator[ChannelEntity]].validate(channel, state, result)
      case alias: EntityInstanceAlias => summon[DialsValidator[EntityInstanceAlias]].validate(alias, state, result)
      case group: GroupEntity         => summon[DialsValidator[GroupEntity]].validate(group, state, result)
      case st: StateEntity         => summon[DialsValidator[StateEntity]].validate(st, state, result)
      case _ =>
        logger.error(s"Unknown DialsEntity type: $entity")
        result
    }
}

