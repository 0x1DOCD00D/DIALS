package Validation

import scala.collection.immutable.{Map, Set}
import cats.Monoid
import cats.implicits._

case class ValidationState(
                            agentNameToHashes: Map[String, Set[String]] = Map.empty,
                            channelNameToHashes: Map[String, Set[String]] = Map.empty,
                            groupNameToHashes: Map[String, Set[String]] = Map.empty,
                            messageNameToHashes: Map[String, Set[String]] = Map.empty,
                            resourceNameToHashes: Map[String, Set[String]] = Map.empty,
                            aliasNameToHashes: Map[String, Map[String, Set[String]]] = Map.empty,
                            visitedEntities: Set[String] = Set.empty
                          ) extends VisitorState {
  override def toString: String =
    s"ValidationState(\n" +
      s"  agentNameToHashes = $agentNameToHashes,\n" +
      s"  channelNameToHashes = $channelNameToHashes,\n" +
      s"  groupNameToHashes = $groupNameToHashes,\n" +
      s"  messageNameToHashes = $messageNameToHashes,\n" +
      s"  resourceNameToHashes = $resourceNameToHashes,\n" +
      s"  aliasNameToHashes = $aliasNameToHashes,\n" +
      s"  visitedEntities = $visitedEntities\n" +
      ")"
}

object ValidationState {
  val empty: ValidationState = ValidationState()

  implicit val validationStateMonoid: Monoid[ValidationState] = Monoid.instance(
    empty,
    (x, y) => ValidationState(
      x.agentNameToHashes |+| y.agentNameToHashes,
      x.channelNameToHashes |+| y.channelNameToHashes,
      x.groupNameToHashes |+| y.groupNameToHashes,
      x.messageNameToHashes |+| y.messageNameToHashes,
      x.resourceNameToHashes |+| y.resourceNameToHashes,
      x.aliasNameToHashes |+| y.aliasNameToHashes,
      x.visitedEntities |+| y.visitedEntities
    )
  )
}
