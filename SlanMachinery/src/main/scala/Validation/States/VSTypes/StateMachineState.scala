package Validation.States.VSTypes

import cats.Monoid
import cats.implicits._

case class StateType(
                      stateName: String,
                      transitionTo: Option[String],
                      failStateTo: Option[String],
                    ) {
  override def toString: String =
    s"StateType($stateName, $transitionTo, $failStateTo)"
}

case object StateType {

  val empty: StateType = StateType("", None, None)

  implicit val stateTypeMonoid: Monoid[StateType] = Monoid.instance(
    empty,
    (x, y) => StateType(
      if (x.stateName.nonEmpty) x.stateName else y.stateName,  // Pick first non-empty stateName
      x.transitionTo.orElse(y.transitionTo),                   // Take first non-empty transition
      x.failStateTo.orElse(y.failStateTo),                     // Take first non-empty fail state
    )
  )
}

case class StateMachineState(
                              agents: List[String],
                              agentStateMachine: Map[String, Set[StateType]]
                            ) {
  override def toString: String = s"StateMachineState($agentStateMachine) + Agents($agents)"
}

case object StateMachineState {

  val empty: StateMachineState = StateMachineState(List(), Map())

  implicit val stateMachineStateMonoid: Monoid[StateMachineState] = Monoid.instance(
    empty,
    (x, y) => StateMachineState(
      (y.agents |+| x.agents).distinct,  // remove duplicates
      (x.agentStateMachine |+| y.agentStateMachine).map {
        case (agent, states) => agent -> states
      }
    )
  )
}
