package Validation.EntityValidation.Agent

import GenericDefinitions.{AgentEntity, StateEntity, ConditionTypeEnum}
import Validation.EntityValidation.Agent.AgentValidations.AgentValidation
import Validation.Results.ValidationResult
import Validation.States.ValidationState

import scala.collection.mutable

object StateMachineValidations {


  /** Ensures all states in the agent entity have outgoing transitions */
  private def checkWellDefinedOutgoingTransitions(agent: AgentEntity, state: ValidationState): ValidationResult = {
    val issues = agent.getStates.collect {
      case s if !agent.getTransitions.contains(s.name) =>
        s"State '${s.name}' has no outgoing transitions, possibly a dead-end state."
    }

    if (issues.isEmpty) ValidationResult.valid
    else ValidationResult.fromWarnings(issues)
  }

  /** Ensures each state has valid expected event-based transitions */
  private def checkExpectedEvents(agent: AgentEntity, state: ValidationState): ValidationResult = {
    val issues = agent.getTransitions.flatMap { case (state, transitions) =>
      val conditions = transitions.values.flatten.map(_.conditionType).toSet
      if (!conditions.contains(ConditionTypeEnum.Always) && !conditions.contains(ConditionTypeEnum.Conditional))
        Some(s"State '$state' lacks valid event-based transitions.")
      else None
    }

    if (issues.isEmpty) ValidationResult.valid
    else ValidationResult.fromWarnings(issues.toList)
  }

  /** Ensures all states are reachable from an initial state */
  private def checkUnreachableStates(agent: AgentEntity, state: ValidationState): ValidationResult = {
    val visited = mutable.Set[String]()

    def dfs(current: String): Unit = {
      if (!visited.contains(current)) {
        visited.add(current)
//        TODO: modify these later for debug logs
        println(s"Visiting: $current")
        println(s"Transitions: ${agent.getTransitions.get(current)}")
        agent.getTransitions.get(current).foreach(_.keys.foreach(dfs))
      }
    }

//  initial state comes from autoTriggeredState or the first state in the list of states
    val initialState = agent.getAutoTriggeredState.getOrElse(agent.getStates.head)


    dfs(initialState.name)
    println(s"Visited: $visited")
//    TODO: create methods for strings which are used to create errors and warnings
    val unreachable = agent.getStates.map(_.name).toSet.diff(visited)
    if (unreachable.isEmpty) ValidationResult.valid
    else ValidationResult.fromErrors(unreachable.toList.map(s => s"State '$s' is unreachable."))
  }


  /** Detects conflicting transitions for the same event or condition */
//  TODO: needs modification
  private def checkConflictingTransitions(agent: AgentEntity, state: ValidationState): ValidationResult = {
    val issues = agent.getTransitions.flatMap { case (currentState, transitions) =>
      val conditionMap = mutable.Map[String, mutable.ListBuffer[String]]()

      for ((nextState, conditions) <- transitions; condition <- conditions) {
        conditionMap.getOrElseUpdate(condition.cSource.get, mutable.ListBuffer()).addOne(nextState)
        if (condition.conditionType == ConditionTypeEnum.Always) {
//         add to all keys
// TODO: side effects, get to this later to remove side effects
          conditionMap.keys.foreach { key =>
            if (key != condition.cSource.get) conditionMap(key).addOne(nextState)
          }
        }
      }

      conditionMap.collect {
        case (cond, nextStates) if nextStates.size > 1 =>
          s"State '$currentState' has conflicting transitions for condition '$cond': ${nextStates.mkString(", ")}."
      }
    }

    if (issues.isEmpty) ValidationResult.valid
    else ValidationResult.fromErrors(issues.toList)
  }


  /** Detects cycles in the state machine */
  private def checkForCycles(agent: AgentEntity, state: ValidationState): ValidationResult = {
    val visited = mutable.Set[String]()
    val stack = mutable.Set[String]()
    val issues = mutable.ListBuffer[String]()

    def dfs(current: String): Boolean = {
      if (stack.contains(current)) {
        issues += s"Cycle detected involving state '$current'."
        return true
      }

      if (!visited.contains(current)) {
        visited += current
        stack += current
        
//        TODO: has a for each remove while optimising
        agent.getTransitions.get(current).foreach { nextStates =>
          nextStates.keys.foreach { nextState =>
            if (dfs(nextState)) {

              return true
            }
          }
        }

        stack -= current
      }

      false
    }

    agent.getStates.foreach(e => dfs(e.name))

    if (issues.isEmpty) ValidationResult.valid
    else ValidationResult.fromWarnings(issues.toList)
  }

  val stateMachineVals: List[AgentValidation] = List(
    checkWellDefinedOutgoingTransitions,
    checkExpectedEvents,
    checkUnreachableStates,
    checkConflictingTransitions,
    checkForCycles
  )

}
