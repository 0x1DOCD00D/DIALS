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
import scala.language.postfixOps

object StateEntity:
  def apply(name: String): StateEntity =
    val newState = new StateEntity(name)
    AgentEntity(newState)
    newState

def always: true = true

class ConditionConstraints(stateEntity: StateEntity, howManyMsgs: Int = 1, c: => Boolean = false):

  def getCondition: () => Boolean = () => c

  infix def `no condition triggers the switch`(cond: => true): FailureCondition =
    val newCond = new ConditionConstraints(stateEntity, Int.MinValue, c)
    stateEntity.conditions = newCond
    AgentEntity(stateEntity)
    new FailureCondition(stateEntity)
    
  infix def when(cond: => Boolean): FailureCondition =
    val newCond = new ConditionConstraints(stateEntity, howManyMsgs, cond)
    stateEntity.conditions = newCond
    AgentEntity(stateEntity)
    new FailureCondition(stateEntity)


class FailureCondition(stateEntity: StateEntity, fs: => Option[StateEntity] = None, d: => Duration = 0.seconds):
  
  def getFs: Option[StateEntity] = fs
  
  infix def timeout(duration: scala.concurrent.duration.Duration): FailureCondition =
    val newFail = new FailureCondition(stateEntity, fs, duration)
    stateEntity.failure = newFail
    AgentEntity(stateEntity)
    newFail

  infix def fail2(failState: => StateEntity): FailureCondition =
    val newFail = new FailureCondition(stateEntity, Some(failState), d)
    stateEntity.failure = newFail
    AgentEntity(stateEntity)
    newFail

  infix def orSwitch2(failState: => StateEntity): ConditionConstraints =
    AgentEntity(stateEntity)
    new ConditionConstraints(stateEntity)


class StateEntity(
                   val name: String,
                   val behaviors: ListBuffer[BehaviorEntity] = ListBuffer(),
                   val onSwitchBehavior: () => Unit = () => {},
                 ) extends DialsEntity:

  private val logger = CreateLogger(classOf[StateEntity])
  private var _conditions: ConditionConstraints = new ConditionConstraints(this)
  private var _doOnFailure: FailureCondition = new FailureCondition(this)

  def conditions: ConditionConstraints = _conditions
  def conditions_=(cond: ConditionConstraints): Unit = _conditions = cond

  def failure: FailureCondition = _doOnFailure
  def failure_=(cond: FailureCondition): Unit = _doOnFailure = cond

  override def toString: String =
    s"StateEntity($name, ${behaviors.map(_.toString).mkString})" +
      s" with conditions ${_conditions.toString}"
  
  infix def behaves(defBehavior: PartialFunction[Any, Unit]): StateEntity =
    AgentEntity.getCurrentAgentState match
      case Some(state) => state
      case None =>
        throw new IllegalStateException(s"The ent ${AgentEntity.getCurrentAgent} has no current state - impossible!")

  infix def onSwitch(onSwitchBehavior: => Unit): StateEntity =
    AgentEntity.getCurrentAgentState match
      case Some(state) =>
        val nState  = new StateEntity(name, behaviors, () => onSwitchBehavior)
        AgentEntity(nState)
        nState

      case None =>
        throw new IllegalStateException(s"The ent ${AgentEntity.getCurrentAgent} has no current state - impossible!")

  infix def periodic(timer: Tuple3[Int, Int, Int]): Unit =
    if ConfigDb.`DIALS.General.debugMode` then logger.info(s"Making the state $name periodic behavior for the ent ${AgentEntity.getCurrentAgent}")
    AgentEntity(this, timer)

  infix def switch2(nextState: => StateEntity): ConditionConstraints =
    if ConfigDb.`DIALS.General.debugMode` then logger.info(s"Switching from state $name to state ${nextState.name} for the ent ${AgentEntity.getCurrentAgent}")
    AgentEntity(this, nextState)
    new ConditionConstraints(this)

  infix def switchOnTimeout(nextState: => StateEntity): FailureCondition =
    if ConfigDb.`DIALS.General.debugMode` then logger.info(s"Switching from state $name to state ${nextState.name} for the ent ${AgentEntity.getCurrentAgent} on timeout only")
    AgentEntity(this, nextState)
    val newCond = new ConditionConstraints(this, Int.MinValue)
    conditions = newCond
    new FailureCondition(this)
