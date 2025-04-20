package Validation.EntityValidation.Channel
import GenericDefinitions.ChannelEntity
import Validation.Results.ValidationResult
import Validation.States.ValidationState
import cats.implicits.*
import Utilz.{ConfigDb, CreateLogger}
import Validation.DialsValidator
import org.slf4j.Logger

object ChannelValidators {
  
  val logger: Logger = CreateLogger(classOf[DialsValidator[ChannelEntity]])
  // ChannelEntity processing and validation
  given DialsValidator[ChannelEntity] with
    def processIR(channel: ChannelEntity, state: ValidationState): ValidationState = {
      val channelHash = channel.hashCode().toString
      if (state.visitedEntities.contains(channelHash)) state
      else {
        if ConfigDb.`DIALS.General.debugMode` then logger.info(s"Processing IR for channel: ${channel.name}")
        val updatedState = state |+| ValidationState.empty.copy(
          visitedEntities = state.visitedEntities + channelHash,
          nameState = state.nameState.copy(
            channelNameToHashes = state.nameState.channelNameToHashes.updated(
              channel.name, state.nameState.channelNameToHashes.getOrElse(channel.name, Set.empty) + channelHash
            ) ,
            messageNameToHashes = state.nameState.messageNameToHashes ++ channel.messages.map { message =>
                message._1.name -> (state.nameState.messageNameToHashes.getOrElse(message._1.name, Set.empty) + message._1.hashCode().toString)
              }.toMap,
          ),
            structState = state.structState.copy(
                channelMessages = state.structState.channelMessages + (channel.name -> channel.messages.map(_._1.name).toSet),
            )
          
        )
        updatedState
      }
    }

    def validate(channel: ChannelEntity, state: ValidationState, result: ValidationResult): ValidationResult = {
      val channelHash = channel.hashCode().toString
      if (result.visitedEntities.contains(channelHash)) result
      else {
        if ConfigDb.`DIALS.General.debugMode` then logger.info(s"Validating channel: ${channel.name}")
        val channelResult = ChannelValidations.validate(channel, state)
        result.copy(visitedEntities = result.visitedEntities :+ channelHash) |+| channelResult
      }
    }

}
