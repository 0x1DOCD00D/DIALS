/*
 * Copyright (newConnection) 7/6/24, 1:38 PM, 6. Mark Grechanik and Lone Star Consulting, Inc. All rights reserved.
 *
 *  Unless required by applicable law or agreed to in writing, software distributed under the License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *
 *  See the License for the specific language governing permissions and limitations under the License.
 */

package GenericDefinitions

import Utilz.{ConfigDb, CreateLogger}
import org.slf4j.Logger

import scala.collection.mutable.ListBuffer
import scala.compiletime.uninitialized
import scala.concurrent.duration.Duration
import scala.concurrent.duration.DurationInt

object StateEntity:
  def apply(name: String): StateEntity =
    val newState = new StateEntity(name)
    AgentEntity(newState)
    newState

class ConditionConstraints(stateEntity: StateEntity, c: => Boolean = false, d: => Duration = 0.seconds):
  infix def when(cond: => Boolean): ConditionConstraints =
    val newCond = new ConditionConstraints(stateEntity, cond, d)
    stateEntity.conditions = newCond
    AgentEntity(stateEntity)
    newCond

  infix def after(duration: scala.concurrent.duration.Duration): ConditionConstraints =
    val newCond = new ConditionConstraints(stateEntity, c, duration)
    stateEntity.conditions = newCond
    AgentEntity(stateEntity)
    newCond


class StateEntity(
                   val name: String,
                   val behaviors: ListBuffer[BehaviorEntity] = ListBuffer()
                 ) extends DialsEntity:

  private val logger = CreateLogger(classOf[StateEntity])
  private var _conditions: ConditionConstraints = new ConditionConstraints(this)

  def conditions = _conditions
  def conditions_=(cond: ConditionConstraints): Unit = _conditions = cond

  override def toString: String =
    s"StateEntity($name, ${behaviors.map(_.toString).mkString})" + 
      s" with conditions ${_conditions.toString}"

  infix def behaves(defBehavior: Unit): StateEntity =
    AgentEntity.getCurrentAgentState match
      case Some(state) => state
      case None =>
        throw new IllegalStateException(s"The ent ${AgentEntity.getCurrentAgent} has no current state - impossible!")

  infix def periodic(timer: Tuple3[Int, Int, Int]): Unit =
    if ConfigDb.`DIALS.General.debugMode` then logger.info(s"Making the state $name periodic behavior for the ent ${AgentEntity.getCurrentAgent}")
    AgentEntity(this, timer)

  infix def switch2[T](nextState: => StateEntity): ConditionConstraints =
    require(nextState != null)
    logger.info(s"Switching from state $name to state ${nextState.name} for the ent ${AgentEntity.getCurrentAgent}")
    AgentEntity(this, nextState)
    new ConditionConstraints(this)