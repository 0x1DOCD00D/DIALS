package GenericDefinitions

import akka.actor.ActorRef

object AkkaMessages {
  /** All resources the agent can touch, and all channels it can send/receive on */
  case class Setup(resources: Map[String, ActorRef],
                   channels : Map[String, ActorRef])

  case class ChannelMessage(sender: ActorRef, dialsMessage: MessageEntity)

  case class ResourceSetMessage(sender: ActorRef, resourceName: String, value: Any)

  case class ResourceGetMessage(sender: ActorRef, resourceName: String)

  case class ResourceGetResponse(sender: ActorRef, resourceName: String, value: Any)
}