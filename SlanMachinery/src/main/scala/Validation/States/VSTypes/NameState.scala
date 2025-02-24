package Validation.States.VSTypes

import cats.Monoid
import cats.implicits._

case class NameState(
                      agentNameToHashes: Map[String, Set[String]] = Map.empty,
                      channelNameToHashes: Map[String, Set[String]] = Map.empty,
                      groupNameToHashes: Map[String, Set[String]] = Map.empty,
                      messageNameToHashes: Map[String, Set[String]] = Map.empty,
                      resourceNameToHashes: Map[String, Set[String]] = Map.empty,
                      aliasNameToHashes: Map[String, Map[String, Set[String]]] = Map.empty,
                    ){
  override def toString: String =
    s"NameState(\n" +
      s"  agentNameToHashes = $agentNameToHashes,\n" +
      s"  channelNameToHashes = $channelNameToHashes,\n" +
      s"  groupNameToHashes = $groupNameToHashes,\n" +
      s"  messageNameToHashes = $messageNameToHashes,\n" +
      s"  resourceNameToHashes = $resourceNameToHashes,\n" +
      s"  aliasNameToHashes = $aliasNameToHashes\n" +
      ")"

}

object NameState {
  val empty: NameState = NameState()

  implicit val nameStateMonoid: Monoid[NameState] = Monoid.instance(
    empty,
    (x, y) => NameState(
      x.agentNameToHashes |+| y.agentNameToHashes,
      x.channelNameToHashes |+| y.channelNameToHashes,
      x.groupNameToHashes |+| y.groupNameToHashes,
      x.messageNameToHashes |+| y.messageNameToHashes,
      x.resourceNameToHashes |+| y.resourceNameToHashes,
      x.aliasNameToHashes |+| y.aliasNameToHashes
    )
  )
}

