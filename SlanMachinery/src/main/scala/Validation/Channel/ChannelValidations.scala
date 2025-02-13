package Validation.Channel

import GenericDefinitions.ChannelEntity
import Validation.{ValidationResult, ValidationState}
import cats.implicits._

object ChannelValidations {
  // A type alias: function (ChannelEntity, ValidationState) => ValidationResult
  type ChannelValidation = (ChannelEntity, ValidationState) => ValidationResult

  private def checkNameNotEmpty(channel: ChannelEntity, state: ValidationState): ValidationResult = {
    if (channel.name.trim.nonEmpty) {
      ValidationResult.valid
    } else {
      ValidationResult.fromError("Channel name cannot be empty.")
    }
  }

  private def checkMessagesSize(channel: ChannelEntity, state: ValidationState): ValidationResult = {
    // Just an example; adjust the message/condition as needed.
    if (channel.messages.nonEmpty) {
      ValidationResult.valid
    } else {
      ValidationResult.fromError(s"No messages in the channel ${channel.name}. Possibly not defined or removed.")
    }
  }

  private def checkDuplicateNames(channel: ChannelEntity, state: ValidationState): ValidationResult = {
    val channelName = channel.name
    if (state.nameState.channelNameToHashes(channelName).size > 1) {
      ValidationResult.fromError(s"Duplicate channel name found: $channelName")
    } else {
      ValidationResult.valid
    }
  }

  /**
   * List of all Channel-level validations
   * (Note: checkCurrentState is currently included twice—remove duplicates if unintended).
   */
  private val allValidations: List[ChannelValidation] = List(
    checkNameNotEmpty,
    checkMessagesSize,
    checkDuplicateNames
  )

  /**
   * Applies all channel validations and accumulates errors/warnings
   */
  def validate(channel: ChannelEntity, state: ValidationState): ValidationResult = {
    allValidations
      .map(fn => fn(channel, state))
      .combineAll
  }
}
