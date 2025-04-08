package Validation.EntityValidation.Resource

import Validation.Results.ValidationResult
import Validation.States.ValidationState
import GenericDefinitions.{ResourceEntity, DialsEntity, StateEntity}
import Utilz.CreateLogger
import cats.implicits.*
import Validation.DialsValidator

object ResourceValidators {

  val logger = CreateLogger(classOf[DialsValidator[ResourceEntity]])
  // also add the field resources object to the state machine 
  // ResourceEntity processing and validation
  given DialsValidator[ResourceEntity] with
    def processIR(resource: ResourceEntity, state: ValidationState): ValidationState = {
      val agent = state.smState.agents.head
      val updatedState = state |+| ValidationState.empty.copy(
        visitedEntities = state.visitedEntities + resource.hashCode().toString,
        nameState = state.nameState.copy(
          resourceNameToHashes = state.nameState.resourceNameToHashes.updated(
            agent, state.nameState.resourceNameToHashes.getOrElse(agent, Map.empty).updated(
              resource.name, state.nameState.resourceNameToHashes.getOrElse(agent, Map.empty).getOrElse(resource.name, Set.empty) + resource.hashCode().toString
            )
          )
        ),
      )
      
//      for field resources

      val allFieldResources = resource.collectAllFieldResources
      
      val updatedState2 = allFieldResources.foldLeft(updatedState) { (accState, fieldResource) =>
        val fieldResourceHash = fieldResource.hashCode().toString
        accState |+| ValidationState.empty.copy(
          visitedEntities = accState.visitedEntities + fieldResourceHash,
          nameState = accState.nameState.copy(
            resourceNameToHashes = accState.nameState.resourceNameToHashes.updated(
              agent, accState.nameState.resourceNameToHashes.getOrElse(agent, Map.empty).updated(
                fieldResource.name, accState.nameState.resourceNameToHashes.getOrElse(agent, Map.empty).getOrElse(fieldResource.name, Set.empty) + fieldResourceHash
              )
            )
          )
        )
      }
      
      

      updatedState2
    }

    def validate(resource: ResourceEntity, state: ValidationState, result: ValidationResult): ValidationResult = {
      return result
    }


}
