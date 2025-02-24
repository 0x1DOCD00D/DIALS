package Validation.EntityValidation.Groups
import GenericDefinitions.GroupEntity
import Validation.DialsValidator
import Validation.States.ValidationState
import Validation.Results.ValidationResult
import Utilz.CreateLogger
import cats.implicits._

object GroupValidators {
  val logger = CreateLogger(classOf[DialsValidator[GroupEntity]])

  // GroupEntity processing and validation
  given DialsValidator[GroupEntity] with
    def processIR(group: GroupEntity, state: ValidationState): ValidationState = {
      val groupHash = group.hashCode().toString
      if (state.visitedEntities.contains(groupHash)) state
      else {
        logger.info(s"Processing IR for group")
        state |+| ValidationState.empty.copy(
          visitedEntities = state.visitedEntities + groupHash
        )
      }
    }

    def validate(group: GroupEntity, state: ValidationState, result: ValidationResult): ValidationResult = {
      val groupHash = group.hashCode().toString
      if (result.visitedEntities.contains(groupHash)) result
      else {
        logger.info(s"Validating group")
        result.copy(visitedEntities = result.visitedEntities :+ groupHash)
      }
    }

}
