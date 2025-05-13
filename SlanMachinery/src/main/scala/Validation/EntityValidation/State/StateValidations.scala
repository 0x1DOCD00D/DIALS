package Validation.EntityValidation.State
import GenericDefinitions.StateEntity
import Validation.Results.ValidationResult
import Validation.States.ValidationState
import cats.implicits.*
import StateValidationMessages._

object StateValidations {

  type StateValidation = (StateEntity, ValidationState) => ValidationResult

  private def checkNameNotEmpty(state: StateEntity, validationState: ValidationState): ValidationResult = {
    if (state.name.trim.nonEmpty) {
      ValidationResult.valid
    } else {
      ValidationResult.fromError(emptyName)
    }
  }

  private def checkStateNotEmpty(stateEntity: StateEntity, validationState: ValidationState): ValidationResult = {
    if (stateEntity.behaviors.isEmpty && stateEntity.onSwitchBehavior.isEmpty) {
      ValidationResult.fromError(emptyState.format(stateEntity.name))
    } else {
      ValidationResult.valid
    }
  }

  private val allValidations: List[StateValidation] = List(
    checkNameNotEmpty,
    checkStateNotEmpty
  )

  def validate(st: StateEntity, state: ValidationState): ValidationResult = {
    allValidations.map(_(st, state)).combineAll
  }

}
