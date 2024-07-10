/*
 * Copyright (c) 7/6/24, 10:33 AM, 6. Mark Grechanik and Lone Star Consulting, Inc. All rights reserved.
 *
 *  Unless required by applicable law or agreed to in writing, software distributed under the License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *
 *  See the License for the specific language governing permissions and limitations under the License.
 */

package GenericDefinitions

import Utilz.CreateLogger

import scala.collection.mutable
import scala.{:+, Dynamic}
import scala.collection.mutable.ListBuffer
import scala.language.dynamics
import scala.language.postfixOps

/*
* An agent represents an FSM that is a relation between states defined by transitions.
* In turn, each state is linked to specific behaviors that are executed when the agent is in that state.
* Since each behavior is triggered by messages of certain types there are many behaviors that can be executed in a given state
* depending on messages that are received by this agent.
  (agent process1) has {
  (state startState) behaves {
    (behavior behavior1) contains {
      println(s"agent ${mutableRef.ref.get} behavior in the init state")
    };
    (behavior behavior2) contains {
      println(s"agent ${mutableRef.ref.get} behavior 2 in the init state")
    }
  } transitions to(state newState);
  (state newState) behaves {
    (behavior behavior1) contains {
      println(s"agent ${mutableRef.ref.get} behavior in the new state")
    }
  } transitions to(state newState1);
  (state newState1) transitions to(state newState2)
}

(agent process2) has {
  (behavior behavior1) contains {
      println(s"agent ${mutableRef.ref.get} behavior in the new state")
    }
  (resource resource1)
}
* Agent process2 is a simple agent that does not have states and transitions. It is defined by a single behavior that is executed when, for example, an agent receives certain messages
* and a single resource that is used by this agent. Evaluating (agent process1) results in an object of AgentEntity whose method, has takes a block of code that contains the definition of some FSM as
* states with behaviors and their transitions. The initial state is designated with the case object InitialState and the transitions are defined
* using the method, transitions. Some behaviors may be defined locally within the block of code inside the block of code that is passed to the method, has.
* Once a block of code is passed to the method has it is executed and a state machine is extracted. That is, each (state <name>) entry is executed
* and it creates an object of the type StateEntity that is populated with the information about the behavior in this state that is passed to the method, behaves. 
* The orchestration of this evaluation ensures that the context reference is never modified concurrently, but only in timed sequence of the execution steps. 
* That is, when (agent process1) is executed it creates an object of AgentEntity and makes it the current top context agent in the AgentEntity object.  
* Next, the method has is called and it executes the block of code that is passed to it. Each entry in this block of code declares either a state
* or a resource. Executing each entry creates an object of either StateEntity or ResourceEntity and populates it with the information about this entity
* just like it is done with the object of AgentEntity. 
* One frequently used solution is a special combinator, usually ~ that combines different entities in a single context, e.g., a list that contains
* the elements that are representations of these entities. The problem is to make sure that all keywords and blocks of code that finish a declarative
* statement result in an object of the type that implements the method ~. Using semicolon seems to be a natural way to combine declarations.
* When an object of StateEntity or ResourceEntity is created it is added to the list of states/resources of the current top context agent.
*/
object AgentEntity:
  private val logger = CreateLogger(classOf[AgentEntity])
  private val agents: ListBuffer[AgentEntity] = ListBuffer()

  override def toString: String = 
    s"All agents: ${agents.toList.map(_.name)} with the following breakdown:\n" + agents.map(_.toString).mkString(";\n\n")

  def apply(): List[String] = agents.map(_.name).toList
  
  def apply(name: String): AgentEntity =
    logger.info(s"Creating an agent entity named $name")
    val found = agents.toList.find(a => a.name == name)
    if found.isDefined then
      logger.error(s"Agent $name already exists. Adding more definitions to the existing agent.")
      val agt = found.get
      val (l, r) = agents.partition(a => a.name == name)
      agents.clear()
      agents.appendAll(r)
      agents.prependAll(l)
      agt
    else
      val agent = new AgentEntity(name)
      agents.prependAll(List(agent))
      agent

  def getState(name: String): Option[StateEntity] = agents.headOption.flatMap(_.getStates.find(s => s.name == name))
  
  def apply(stateEntity: StateEntity): Unit =
    logger.info(s"Creating a state entity for agent ${agents.head.name}: ${stateEntity.toString}")
    val lst = agents.toList 
    if lst.isEmpty then 
      throw new IllegalStateException(s"No agent is defined even though the state is specified: ${stateEntity.name}")
    else if !lst.head.states.exists(s => s.name == stateEntity.name) then
      logger.info(s"Creating the state ${stateEntity.name} under the agent ${lst.head.name}")
      agents.head.states.prepend(stateEntity)
      agents.head.currentState = Some(stateEntity)
    else
      val oldState = lst.head.states.find(s => s.name == stateEntity.name).get
      logger.info(s"Replacing state ${oldState.toString} in agent ${lst.head.name} with a new state entity ${stateEntity.toString}")
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

  def apply(action: BehaviorEntity): Unit =
    val lst = agents.toList
    if lst.isEmpty then throw new IllegalStateException(s"No agent is defined even though the behavior is specified: ${action.name}")
    else if agents.head.currentState.isDefined then
      val state = agents.head.currentState.get
      logger.info(s"Creating a behavior entity named ${action.name} under the agent ${lst.head.name} for its state ${state.name}")
      AgentEntity(new StateEntity(state.name, action :: state.behaviors))
    else
      throw new IllegalStateException(s"No state is defined even though the behavior is specified: ${action.name}")
  
  def apply(resourceEntity: ResourceEntity): Unit =
    agents.head.resources.append(resourceEntity)

  def getCurrentAgent: Option[String] = agents.headOption.map(_.name)
  def getCurrentAgentState: Option[StateEntity] = agents.headOption.flatMap(_.getCurrentState)
  
class AgentEntity(val name: String) extends DialsEntity:
  private val states: ListBuffer[StateEntity] = ListBuffer()
  private val stateTransitions: mutable.Map[StateEntity, StateEntity] = mutable.Map()
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
      (if states.nonEmpty then states.toList.map(s => s"\nstate ${s.name}: " + s.behaviors.map(b=>b.toString()).mkString(" ")).mkString("\n") else "")

  def getStates: List[StateEntity] = states.toList
  def getCurrentState: Option[StateEntity] = currentState
  def checkIfStateExists(se:StateEntity): Boolean = states.toList.exists(s => s.name == se.name)
  def getResources: List[ResourceEntity] = resources.toList
  
  infix def has[T](defAgent: T): T = defAgent
