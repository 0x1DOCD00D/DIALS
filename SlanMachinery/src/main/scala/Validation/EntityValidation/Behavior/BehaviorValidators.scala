package Validation.EntityValidation.Behavior

import GenericDefinitions.BehaviorEntity
import Utilz.CreateLogger
import Validation.DialsValidator
import Validation.Results.ValidationResult
import Validation.States.ValidationState

object BehaviorValidators {
  val logger = CreateLogger(classOf[DialsValidator[BehaviorEntity]])

  given DialsValidator[BehaviorEntity] with
    def processIR(entity: BehaviorEntity, state: ValidationState): ValidationState =
      //      multiple states can be used in different agents so we need to process this multiple times
      val agent = state.smState.agents.head
      //        can utilize this to assign any information to the agent state machine
      state

    def validate(entity: BehaviorEntity, state: ValidationState, result: ValidationResult): ValidationResult = ???


}
