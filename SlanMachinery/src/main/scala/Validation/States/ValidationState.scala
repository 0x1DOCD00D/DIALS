package Validation.States

import Validation.States.VSTypes.{NameState, StructureState, StateMachineState}
import Validation.Visitors.VisitorState

import scala.collection.immutable.{Map, Set}
import cats.Monoid
import cats.implicits.*


case class ValidationState(
                            nameState: NameState = NameState(),
                            structState: StructureState = StructureState(),
                            smState: StateMachineState = StateMachineState(List(),Map()),
                            visitedEntities: Set[String] = Set.empty
                          ) extends VisitorState {
  override def toString: String =
    s"ValidationState(\n" +
      s"  nameState = $nameState,\n" +
      s"  visitedEntities = $visitedEntities\n" +
      s"  structState = $structState\n" +
      s"  smState = $smState\n" +
      ")"

}

object ValidationState {
  val empty: ValidationState = ValidationState()

  implicit val validationStateMonoid: Monoid[ValidationState] = Monoid.instance(
    empty,
    (x, y) => ValidationState(
      x.nameState |+| y.nameState,
      x.structState |+| y.structState,
      x.smState |+| y.smState,
      x.visitedEntities |+| y.visitedEntities

    )
  )
}
