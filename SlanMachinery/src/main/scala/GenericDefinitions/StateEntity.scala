/*
 * Copyright (c) 7/6/24, 1:38 PM, 6. Mark Grechanik and Lone Star Consulting, Inc. All rights reserved.
 *
 *  Unless required by applicable law or agreed to in writing, software distributed under the License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *
 *  See the License for the specific language governing permissions and limitations under the License.
 */

package GenericDefinitions

import Utilz.{ConfigDb, CreateLogger}
import org.slf4j.Logger

import scala.collection.mutable.ListBuffer

object StateEntity:
  def apply(name: String): StateEntity =
    val newState = new StateEntity(name)
    AgentEntity(newState)
    newState

class StateEntity(val name: String, val behaviors: ListBuffer[BehaviorEntity] = ListBuffer()) extends DialsEntity:
  private val logger = CreateLogger(classOf[StateEntity])

  override def toString: String =
    s"StateEntity($name, ${behaviors.map(_.toString).mkString})"

  infix def behaves(defBehavior: Unit): StateEntity =
    AgentEntity.getCurrentAgentState match
      case Some(state) => state
      case None =>
        throw new IllegalStateException(s"The agent ${AgentEntity.getCurrentAgent} has no current state - impossible!")

  infix def periodic(timer: Tuple3[Int, Int, Int]): Unit =
    if ConfigDb.`DIALS.General.debugMode` then logger.info(s"Making the state $name periodic behavior for the agent ${AgentEntity.getCurrentAgent}")
    AgentEntity(this, timer)

  infix def switch2[T](nextState: => StateEntity): Unit =
    require(nextState != null)
    logger.info(s"Switching from state $name to state ${nextState.name} for the agent ${AgentEntity.getCurrentAgent}")
    AgentEntity(this, nextState)