package Validation.EntityValidation.Model

import GenericDefinitions.{CompletedChain, Connection, DialsEntity, ModelEntity}
import Validation.States.ValidationState
import Validation.Results.ValidationResult
import Validation.DialsValidator
import Validation.Utils.ExtractUtils.{extractMessages, extractName}
import org.slf4j.Logger
import cats.implicits.*
import GenericDefinitions.ModelEntity.DIRECTION
import Validation.States.VSTypes.StructureState
import Utilz.{ConfigDb, CreateLogger}

object ModelValidators {
  private def processConnection(c: Connection, currentState: ValidationState): ValidationState = c match {
    case CompletedChain(from, to, channel, dir) =>
      val fromState = summon[DialsValidator[DialsEntity]].processIR(from._1, currentState)
      val toState = summon[DialsValidator[DialsEntity]].processIR(to._1, fromState)
      val channelState = summon[DialsValidator[DialsEntity]].processIR(channel, toState)

      // Extract entity names
      val fromName = extractName(from._1)
      val toName = extractName(to._1)
      val channelName = channel.name

      // Define updates using Monoids
      val directionState = dir match {
        case DIRECTION.BIDIRECTIONAL =>
          StructureState(
            outGoingChannels = Map(fromName -> Set(channelName), toName -> Set(channelName)),
            incomingChannels = Map(fromName -> Set(channelName), toName -> Set(channelName)),
            agentIncomingMessages = Map(fromName -> extractMessages(channel), toName -> extractMessages(channel)),
            agentOutgoingMessages = Map(fromName -> extractMessages(channel), toName -> extractMessages(channel))
          )

        case DIRECTION.LEFT2RIGHT =>
          StructureState(
            outGoingChannels = Map(fromName -> Set(channelName)),
            incomingChannels = Map(toName -> Set(channelName)),
            agentIncomingMessages = Map(toName -> extractMessages(channel)),
            agentOutgoingMessages = Map(fromName -> extractMessages(channel))
          )

        case DIRECTION.RIGHT2LEFT =>
          StructureState(
            outGoingChannels = Map(toName -> Set(channelName)),
            incomingChannels = Map(fromName -> Set(channelName)),
            agentIncomingMessages = Map(fromName -> extractMessages(channel)),
            agentOutgoingMessages = Map(toName -> extractMessages(channel))
          )
      }

      // Merge states using Monoid composition
      channelState |+| ValidationState(structState = directionState)

    case _ =>
      if ConfigDb.`DIALS.General.debugMode` then logger.error(s"Not a completed chain")
      currentState
  }

  private def validateConnection(c: Connection, state: ValidationState, result: ValidationResult): ValidationResult = c match {
    case CompletedChain(from, to, channel, dir) =>
      val fromState = summon[DialsValidator[DialsEntity]].validate(from._1, state, result)
      val toState = summon[DialsValidator[DialsEntity]].validate(to._1, state, fromState)
      val channelState = summon[DialsValidator[DialsEntity]].validate(channel, state, toState)
      channelState


    case _ =>
      if ConfigDb.`DIALS.General.debugMode` then logger.error(s"Not a completed chain")
      result
  }

  val logger: Logger = CreateLogger(classOf[DialsValidator[ModelEntity]])


  // ModelEntity processing and validation
  given DialsValidator[ModelEntity] with
    def processIR(model: ModelEntity, state: ValidationState): ValidationState = {
      val modelHash = model.hashCode().toString
      if (state.visitedEntities.contains(modelHash)) state
      else {
        if ConfigDb.`DIALS.General.debugMode` then logger.info(s"Processing IR for model: ${model.name}")
        val updatedState = state |+| ValidationState.empty.copy(
          visitedEntities = state.visitedEntities + model.hashCode().toString
        )

        // Process each connection functionally, passing the updated state along
        model.connections.foldLeft(updatedState) { (accState, conn) =>
          processConnection(conn, accState)
        }
      }
    }

    def validate(model: ModelEntity, state: ValidationState, result: ValidationResult): ValidationResult = {
      val modelHash = model.hashCode().toString
      if (result.visitedEntities.contains(modelHash)) result
      else {
        if ConfigDb.`DIALS.General.debugMode` then logger.info(s"Validating model: ${model.name}")
        val updatedRes = result.copy(visitedEntities = result.visitedEntities :+ modelHash)

        // Process each connection functionally, passing the updated state along
        model.connections.foldLeft(updatedRes) { (accRes, conn) =>
          validateConnection(conn, state, accRes)
        }

      }
    }
}
