package Validation

import scala.collection.immutable.{Map, Set}
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



case class ValidationState(
                            nameState: NameState = NameState(),
                            visitedEntities: Set[String] = Set.empty
                          ) extends VisitorState {
  override def toString: String =
    s"ValidationState(\n" +
      s"  nameState = $nameState,\n" +
      s"  visitedEntities = $visitedEntities\n" +
      ")"

}

object ValidationState {
  val empty: ValidationState = ValidationState()

  implicit val validationStateMonoid: Monoid[ValidationState] = Monoid.instance(
    empty,
    (x, y) => ValidationState(
      x.nameState |+| y.nameState,
      x.visitedEntities |+| y.visitedEntities
    )
  )
}
