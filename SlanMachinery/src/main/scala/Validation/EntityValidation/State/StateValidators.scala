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

      if (entity.name == "Wait4ResponsesFromNeighbors"){
        logger.info(s"Processing the state behaviors ${entity.name}")
        entity.behaviors.map(
          b => b.actualActions.map(
            a => {
              println(printAST(a("")))
              println(inspect(a("")))
            }
          )
        )
      }
      state

    def validate(entity: StateEntity, state: ValidationState, result: ValidationResult): ValidationResult = ???

}
