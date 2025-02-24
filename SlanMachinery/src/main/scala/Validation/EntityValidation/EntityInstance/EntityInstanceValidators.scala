package Validation.EntityValidation.EntityInstance

import GenericDefinitions.{EntityInstanceAlias, AgentEntity, DialsEntity}
import Validation.DialsValidator
import Validation.Results.ValidationResult
import Validation.States.ValidationState
import cats.implicits._
import Utilz.CreateLogger



object EntityInstanceValidators {
  val logger = CreateLogger(classOf[DialsValidator[EntityInstanceAlias]])
  given DialsValidator[EntityInstanceAlias] with
    def processIR(alias: EntityInstanceAlias, state: ValidationState): ValidationState = {
      val aliasHash = alias.hashCode().toString
      if (state.visitedEntities.contains(aliasHash)) state
      else {
        val agentName = alias.ent.get.asInstanceOf[AgentEntity].name

        val updatedAliasMapping: Map[String, Set[String]] =
          state.nameState.aliasNameToHashes.getOrElse(agentName, Map.empty) + (
            alias.alias -> (state.nameState.aliasNameToHashes
              .getOrElse(agentName, Map.empty)
              .getOrElse(alias.alias, Set.empty) + aliasHash)
            )

        // Create a new NameState with the updated aliasNameToHashes
        val updatedNameState = state.nameState.copy(
          aliasNameToHashes = state.nameState.aliasNameToHashes.updated(agentName, updatedAliasMapping)
        )

        // Merge the changes into the overall ValidationState using the |+| operator
        val updatedState = state |+| ValidationState.empty.copy(
          visitedEntities = state.visitedEntities + aliasHash,
          nameState = updatedNameState
        )

        summon[DialsValidator[DialsEntity]].processIR(alias.ent.get, updatedState)
      }
    }

    def validate(alias: EntityInstanceAlias, state: ValidationState, result: ValidationResult): ValidationResult = {
      val aliasHash = alias.hashCode().toString
      if (result.visitedEntities.contains(aliasHash)) result
      else {
        logger.info(s"Validating alias: ${alias.alias}")
        val updatedRes = result.copy(visitedEntities = result.visitedEntities :+ aliasHash)
        summon[DialsValidator[DialsEntity]].validate(alias.ent.get, state, updatedRes)
      }
    }

}
