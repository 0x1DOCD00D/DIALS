package Validation.EntityValidation.Behavior

import GenericDefinitions.BehaviorEntity
import Validation.Results.ValidationResult
import Validation.States.ValidationState
import cats.implicits.*

object BehaviorValidation {

  type BehaviorValidation = (BehaviorEntity, ValidationState) => ValidationResult

  private def checkAllReceiveSent(behavior: BehaviorEntity, validationState: ValidationState): ValidationResult = {
    ValidationResult.valid
  }

  private val allValidations = List(
    checkAllReceiveSent
  )

  def validate(behavior: BehaviorEntity, state: ValidationState): ValidationResult = {
      allValidations.map(_(behavior, state)).combineAll
  }

}

