/*
 * Copyright (newConnection) 7/6/24, 10:33 AM, 6. Mark Grechanik and Lone Star Consulting, Inc. All rights reserved.
 *
 *  Unless required by applicable law or agreed to in writing, software distributed under the License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *
 *  See the License for the specific language governing permissions and limitations under the License.
 */

package GenericDefinitions

import GenericDefinitions.AgentEntity.logger
import GenericDefinitions.ModelEntity.DIRECTION.{BIDIRECTIONAL, LEFT2RIGHT, RIGHT2LEFT}
import GenericDefinitions.ModelEntity.createPartialConnection
import Utilz.Constants.SenderAgentID
import Utilz.{ConfigDb, CreateLogger}

import scala.collection.mutable
import scala.{:+, Dynamic}
import scala.collection.mutable.ListBuffer
import scala.language.dynamics
import scala.language.postfixOps

/*
* An ent represents an FSM that is a relation between states defined by transitions.
* In turn, each state is linked to specific behaviors that are executed when the ent is in that state.
* Since each behavior is triggered by messages of certain types there are many behaviors that can be executed in a given state
* depending on messages that are received by this ent.
  (ent process1) has {
  (state startState) behaves {
    (behavior behavior1) does {
      println(s"ent ${mutableRef.ref.get} behavior in the init state")
    };
    (behavior behavior2) does {
      println(s"ent ${mutableRef.ref.get} behavior 2 in the init state")
    }
  } transitions to(state newState);
  (state newState) behaves {
    (behavior behavior1) does {
      println(s"ent ${mutableRef.ref.get} behavior in the new state")
    }
  } transitions to(state newState1);
  (state newState1) transitions to(state newState2)
}

(ent process2) has {
  (behavior behavior1) does {
      println(s"ent ${mutableRef.ref.get} behavior in the new state")
    }
  (resource resource1)
}
* Agent process2 is a simple ent that does not have states and transitions. It is defined by a single behavior that is executed when, for example, an ent receives certain messages
* and a single resource that is used by this ent. Evaluating (ent process1) results in an object of AgentEntity whose method, has takes a block of code that does the definition of some FSM as
* states with behaviors and their transitions. The initial state is designated with the case object InitialState and the transitions are defined
* using the method, transitions. Some behaviors may be defined locally within the block of code inside the block of code that is passed to the method, has.
* Once a block of code is passed to the method has it is executed and a state machine is extracted. That is, each (state <name>) entry is executed
* and it creates an object of the type StateEntity that is populated with the information about the behavior in this state that is passed to the method, behaves.
* The orchestration of this evaluation ensures that the context reference is never modified concurrently, but only in timed sequence of the execution steps.
* That is, when (ent process1) is executed it creates an object of AgentEntity and makes it the current top context ent in the AgentEntity object.
* Next, the method has is called and it executes the block of code that is passed to it. Each entry in this block of code declares either a state
* or a resource. Executing each entry creates an object of either StateEntity or ResourceEntity and populates it with the information about this entity
* just like it is done with the object of AgentEntity.
* One frequently used solution is a special combinator, usually ~ that combines different entities in a single context, e.g., a list that does
* the elements that are representations of these entities. The problem is to make sure that all keywords and blocks of code that finish a declarative
* statement result in an object of the type that implements the method ~. Using semicolon seems to be a natural way to combine declarations.
* When an object of StateEntity or ResourceEntity is created it is added to the list of states/resources of the current top context ent.
*/
object AgentEntity extends EnumeratedNamedEntityInstance:
  private val logger = CreateLogger(classOf[AgentEntity])
  private val agents: ListBuffer[AgentEntity] = ListBuffer()
  private val autoTriggered: mutable.Map[AgentEntity, StateEntity] = mutable.Map()

  override def toString: String =
    s"All agents: ${agents.toList.map(_.name)} with the following breakdown:\n" + agents.map(_.toString).mkString(";\n\n") +
    s"\n\nAliases: ${enumeratedAliases.map(_.toString).mkString(", ")}"

  def resetAll(): Unit =
    agents.clear()
    autoTriggered.clear()

  def apply(): List[String] = agents.map(_.name).toList

  def apply(name: String): AgentEntity =
    if ConfigDb.`DIALS.General.debugMode` then logger.info(s"Creating an ent entity named $name")
    val found = agents.toList.find(a => a.name == name)
    if found.isDefined then
      if ConfigDb.`DIALS.General.debugMode` then logger.info(s"Agent $name is already defined.")
      val agt = found.get
      val (l, r) = agents.partition(a => a.name == name)
      agents.clear()
      agents.appendAll(r)
      agents.prependAll(l)
      joinGroup(agt)
      agt
    else
      val agent = new AgentEntity(name)
      agents.prependAll(List(agent))
      joinGroup(agent)
      agent

  def joinGroup(a: AgentEntity): Unit = if GlobalProcessingState.isGroup then GroupEntity(a)

  def getState(name: String): Option[StateEntity] = agents.headOption.flatMap(_.getStates.find(s => s.name == name))

  def apply(alias: EntityInstanceAlias): Unit = 
    if enumeratedAliases.toList.exists(e => e._1 == alias.alias) then
      logger.warn(s"Agent alias ${alias.alias}.name}")
    else if ConfigDb.`DIALS.General.debugMode` then logger.info(s"Creating an ent alias ${alias.alias} for ent ${alias.ent.getOrElse("ent unknown")}")
    enumeratedAliases.prependAll(List(alias))
    
  def apply(stateEntity: StateEntity): Unit =
    logger.info(s"Creating a state entity for ent ${agents.head.name}: ${stateEntity.toString}")
    val lst = agents.toList
    if lst.isEmpty then
      throw new IllegalStateException(s"No ent is defined even though the state is specified: ${stateEntity.name}")
    else if !lst.head.states.exists(s => s.name == stateEntity.name) then
      logger.info(s"Creating the state ${stateEntity.name} under the ent ${lst.head.name}")
      agents.head.states.prepend(stateEntity)
      agents.head.currentState = Some(stateEntity)
    else
      val oldState = lst.head.states.find(s => s.name == stateEntity.name).get
      logger.info(s"Replacing state ${oldState.toString} in ent ${lst.head.name} with a new state entity ${stateEntity.toString}")
      agents.head.currentState = Some(stateEntity)
      agents.head.states.update(agents.head.states.indexWhere(s => s.name == stateEntity.name), stateEntity)

  def apply(stateEntityFrom: StateEntity, stateEntity2: StateEntity): Unit =
    if agents.head.stateTransitions.contains(stateEntityFrom) then
      if agents.head.stateTransitions(stateEntityFrom) == stateEntity2 then
        logger.warn(s"Transition from state $stateEntityFrom to state $stateEntity2 already exists")
      else
        agents.head.stateTransitions(stateEntityFrom) = stateEntity2
    else
      agents.head.stateTransitions.put(stateEntityFrom, stateEntity2)

  def apply(stateEntity: StateEntity, timer: Tuple3[Int,Int,Int]): Unit =
    if agents.head.periodicBehaviors.contains(stateEntity) then
      logger.warn(s"Periodic behavior for state $stateEntity already exists")
      agents.head.periodicBehaviors(stateEntity) = timer
    else
      agents.head.periodicBehaviors.put(stateEntity, timer)

  def apply(agent: AgentEntity, state: StateEntity): Unit =
    if autoTriggered.contains(agent) then
      logger.warn(s"Auto trigger behavior for ent ${agent.name} already exists, resetting it to the new state ${state.name}")
      autoTriggered(agent) = state
    else
      autoTriggered.put(agent, state)

  def autoTriggeredAgents(): List[(AgentEntity, StateEntity)] = autoTriggered.toList

  def apply(action: BehaviorEntity): Unit =
    val lst = agents.toList
    if lst.isEmpty then throw new IllegalStateException(s"No ent is defined even though the behavior is specified: ${action.name}")
    else if agents.head.currentState.isDefined then
      val state = agents.head.currentState.get
      state.behaviors.find(b => b.name == action.name) match
        case Some(b) =>
          if ConfigDb.`DIALS.General.debugMode` then logger.info(s"Behavior ${action.name} already exists in the state ${state.name} of the ent ${lst.head.name}")
          b.triggerMsgs.prependAll(action.triggerMsgs)
          b.actualActions.prependAll(action.actualActions)
          if ConfigDb.`DIALS.General.debugMode` then logger.info(s"Behavior ${action.name} is updated with ${action.triggerMsgs.toList.length} trigger messages and ${action.actualActions.toList.length} actions")
          if ConfigDb.`DIALS.General.debugMode` then logger.info(s"Behavior ${b.name} contains ${b.triggerMsgs.toList.length} trigger messages and ${b.actualActions.toList.length} actions")
        case None =>
          if ConfigDb.`DIALS.General.debugMode` then logger.info(s"Creating a behavior entity named ${action.name} under the ent ${lst.head.name} for its state ${state.name}")
          state.behaviors.prependAll(List(action))
    else
      throw new IllegalStateException(s"No state is defined even though the behavior is specified: ${action.name}")

  def apply(msgTrigger: MessageEntity): Unit =
    val lst = agents.toList
    if lst.isEmpty then throw new IllegalStateException(s"No ent is defined even though the trigger message is specified: ${msgTrigger.name}")
    else if agents.head.currentState.isDefined then
      val state = agents.head.currentState.get
      val beh = state.behaviors.headOption
      if beh.isDefined then
        logger.info(s"Triggering message ${msgTrigger.name} for the behavior ${beh.get.name} under the ent ${lst.head.name} for its state ${state.name}")
        beh.get.triggerMsgs.prependAll(List(msgTrigger))
      else
        throw new IllegalStateException(s"No behavior is defined even though the trigger message is specified: ${msgTrigger.name}")
    else
      throw new IllegalStateException(s"No state is defined even though the trigger message is specified: ${msgTrigger.name}")


  def apply(resourceEntity: ResourceEntity): Unit =
    if agents.isEmpty then throw new IllegalStateException(s"No ent is defined even though the resource is specified: ${resourceEntity.name}")
    else
      logger.info(s"Creating a resource entity named ${resourceEntity.name} under the ent ${agents.head.name}")
      agents.head.resources.prependAll(List(resourceEntity))

  def getCurrentAgent: Option[String] = agents.headOption.map(_.name)
  def getCurrentAgentState: Option[StateEntity] = agents.headOption.flatMap(_.getCurrentState)

case object SenderAgent extends AgentEntity(SenderAgentID)

class AgentEntity(val name: String) extends DialsEntity:
  private val states: ListBuffer[StateEntity] = ListBuffer()
  private val stateTransitions: mutable.Map[StateEntity, StateEntity] = mutable.Map()
  private val periodicBehaviors: mutable.Map[StateEntity, Tuple3[Int,Int,Int]] = mutable.Map()
  private val resources: ListBuffer[ResourceEntity] = ListBuffer()
  private var currentState: Option[StateEntity] = None

  override def toString: String =
    (if states.isEmpty then s"Agent $name has no states"
    else
      if states.isEmpty then s"Agent $name has no states" else s"Agent $name has states: ${states.map(_.name).mkString(", ")}")
    + (if resources.isEmpty then " and no resources"
        else s" and resources are ${resources.map(_.name).mkString}")
    +
      ( if stateTransitions.isEmpty then " and no state transitions\n"
        else s" and state transitions are ${stateTransitions.map{case (k, v) => s"${k.name} -> ${v.name}"}.mkString("; ")}")
    +
      (if states.nonEmpty then states.toList.map(s => s"\nstate ${s.name}: " + s.toString()) else "") +
      (if periodicBehaviors.isEmpty then " and no periodic behaviors\n"
        else s" and periodic behaviors are ${periodicBehaviors.map{case (k, v) => s"${k.name} -> $v"}.mkString("; ")}")

  def getStates: List[StateEntity] = states.toList
  def getCurrentState: Option[StateEntity] = currentState
  def checkIfStateExists(se:StateEntity): Boolean = states.toList.exists(s => s.name == se.name)
  def getResources: List[ResourceEntity] = resources.toList

  infix def autotrigger(state: StateEntity): Unit =
    if ConfigDb.`DIALS.General.debugMode` then logger.info(s"Setting the state ${state.name} to autotrigger for the ent $name")
    AgentEntity(this, state)

  infix def switch2(nextState: StateEntity): Unit =
    require(nextState != null)
    val currAgentState = AgentEntity.getCurrentAgentState
    if currAgentState.isDefined then
      logger.info(s"Switching from state ${currAgentState.get.name} to the state ${nextState.name} for the ent ${AgentEntity.getCurrentAgent}")
      AgentEntity(currAgentState.get, nextState)
      ()
    else throw new IllegalStateException(s"The ent ${AgentEntity.getCurrentAgent} has no current state - totally impossible!")

  infix def joins(group: => GroupEntity): Unit =
    if GlobalProcessingState.isGroup then
      logger.error(s"The ent $name cannot join a group because it is already a member of a group")
    else group.comprises(this)

  infix def leaves(group: => GroupEntity): Unit =
    if GlobalProcessingState.isGroup then
      logger.error(s"The ent $name cannot leave a group because it's being defined as a member of a group")
    else group.removeAgent(this)

  infix def has[T](defAgent: => T): AgentEntity =
    GlobalProcessingState(this) match
      case Left(errorMsg) =>
        logger.error(errorMsg)
      case Right(obj) =>
        defAgent
        GlobalProcessingState(NoEntity) match
          case Left(errMsg) => logger.error(errMsg)
          case Right(_) =>
            if ConfigDb.`DIALS.General.debugMode` then logger.info(s"Setting the global processing state to $obj")
    this
