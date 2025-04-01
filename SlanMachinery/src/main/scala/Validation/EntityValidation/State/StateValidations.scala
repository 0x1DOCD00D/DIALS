package Validation.EntityValidation.State
import GenericDefinitions.StateEntity
import Validation.Results.ValidationResult
import Validation.States.ValidationState
import cats.implicits.*

object StateValidations {

  type StateValidation = (StateEntity, ValidationState) => ValidationResult

  private def checkNameNotEmpty(state: StateEntity, validationState: ValidationState): ValidationResult = {
    if (state.name.trim.nonEmpty) {
      ValidationResult.valid
    } else {
      ValidationResult.fromError("State name cannot be empty.")
    }
  }

  private def checkStateNotEmpty(stateEntity: StateEntity, validationState: ValidationState): ValidationResult = {
    if (stateEntity.behaviors.isEmpty && stateEntity.onSwitchBehavior.isEmpty) {
      ValidationResult.fromError(s"State ${stateEntity.name} is empty. Possibly not defined or removed.")
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
