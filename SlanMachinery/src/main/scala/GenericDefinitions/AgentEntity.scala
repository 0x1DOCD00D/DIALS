/*
 * Copyright (c) 7/6/24, 10:33 AM, 6. Mark Grechanik and Lone Star Consulting, Inc. All rights reserved.
 *
 *  Unless required by applicable law or agreed to in writing, software distributed under the License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *
 *  See the License for the specific language governing permissions and limitations under the License.
 */

package GenericDefinitions

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
* 
* 
*      */
object AgentEntity:
  private val agents: ListBuffer[AgentEntity] = ListBuffer()

  def apply(): List[String] = agents.map(_.name).toList
  
  def apply(name: String): AgentEntity = 
    val agent = new AgentEntity(name)
    agents.prependAll(List(agent))
    agent

class AgentEntity(val name: String) extends DialsEntity:
  private val states: ListBuffer[StateEntity] = ListBuffer()
  private val resources: ListBuffer[ResourceEntity] = ListBuffer()
  
  def getStates: List[StateEntity] = states.toList
  def getResources: List[ResourceEntity] = resources.toList
  
  infix def has[T](defAgent: T): T =
    val x = 1  
    defAgent