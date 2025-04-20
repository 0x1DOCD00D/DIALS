package Validation.States.VSTypes

import GenericDefinitions.MessageEntity
import cats.Monoid
import cats.implicits.*

case class StructureState(
                           outGoingChannels: Map[String, Set[String]] = Map.empty,
                           incomingChannels: Map[String, Set[String]] = Map.empty,
                           agentIncomingMessages: Map[String, Set[MessageEntity]] = Map.empty,
                           agentOutgoingMessages: Map[String, Set[MessageEntity]] = Map.empty,
                           channelMessages: Map[String, Set[String]] = Map.empty,
                           testStruct: Map[String, Map[String,Set[String]]] = Map.empty
                         ) {
  override def toString: String =
    s"StructureState(\n" +
      s"  OutGoingChannels = $outGoingChannels,\n" +
      s"  IncomingChannels = $incomingChannels,\n" +
      s"  AgentIncomingMessages = $agentIncomingMessages,\n" +
      s"  AgentOutgoingMessages = $agentOutgoingMessages,\n" +
        s"  ChannelMessages = $channelMessages,\n" +
      s"  TestStruct = $testStruct\n" +
      ")"
}


object StructureState{
  val empty: StructureState = StructureState()

  implicit val structureStateMonoid: Monoid[StructureState] = Monoid.instance(
    empty,
    (x, y) => StructureState(
      x.outGoingChannels |+| y.outGoingChannels,
      x.incomingChannels |+| y.incomingChannels,
      x.agentIncomingMessages |+| y.agentIncomingMessages,
      x.agentOutgoingMessages |+| y.agentOutgoingMessages,
      x.channelMessages |+| y.channelMessages,
      x.testStruct |+| y.testStruct
    )
  )
}
