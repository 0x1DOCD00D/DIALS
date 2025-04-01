package Validation.EntityValidation.State
import GenericDefinitions.StateEntity
import Validation.Results.ValidationResult
import Validation.States.ValidationState
import cats.implicits.*
import Utilz.CreateLogger
import Validation.DialsValidator
import Validation.ReflectionLib.ASTPrinter.printAST
import Validation.ReflectionLib.IdentInspector.inspect

object StateValidators {
  val logger = CreateLogger(classOf[DialsValidator[StateEntity]])

  given DialsValidator[StateEntity] with
    def processIR(entity: StateEntity, state: ValidationState): ValidationState =
//      multiple states can be used in different agents so we need to process this multiple times
      val agent = state.smState.agents.head
//        can utilize this to assign any information to the agent state machine
      state

    def validate(entity: StateEntity, state: ValidationState, result: ValidationResult): ValidationResult =
      logger.info(s"Validating state: ${entity.name}")
      StateValidations.validate(entity, state) |+| result

}
