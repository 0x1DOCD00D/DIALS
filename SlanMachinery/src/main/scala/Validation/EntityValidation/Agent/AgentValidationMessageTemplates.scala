package Validation.EntityValidation.Agent

object AgentValidationMessageTemplates {
  val emptyAgentName = "Agent name cannot be empty."
  val emptyAgent = "Agent %s is empty. Possibly not defined or removed."
  val unhandledMessagesString = "Agent %s has unhandled messages: %s"
  val duplicateAgentName = "Duplicate agent name found: %s"
  val duplicateResourceNames = "Duplicate resource names found: %s"
  val outOfScopeResources = "Agent %s has out-of-scope resource accesses: %s"

  val stateNoOutgoing = "State '%s' has no outgoing transitions, possibly a dead-end state."
  val stateMissingEvents = "State '%s' lacks valid event-based transitions."
  val stateUnreachable = "State '%s' is unreachable."
  val stateConflictingTransitions = "State '%s' has conflicting transitions for condition '%s': %s."
  val stateCycleDetected = "Cycle detected involving state '%s'."
  val invalidSendChannel = "Invalid send channel or message in agent '%s' for channel -> message pairs ('%s' '%s') ."
  val invalidChannelComm = "Invalid channel communication in agent '%s' for channel/s '%s'."
  val invalidMessage = "Invalid message used in agent '%s' for message/s '%s'."
}
