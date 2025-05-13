/*
 * Copyright (newConnection) 7/6/24, 1:38 PM, 6. Mark Grechanik and Lone Star Consulting, Inc. All rights reserved.
 *
 *  Unless required by applicable law or agreed to in writing, software distributed under the License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *
 *  See the License for the specific language governing permissions and limitations under the License.
 */

package GenericDefinitions


import Utilz.{ConfigDb, CreateLogger}
import scala.collection.mutable.ListBuffer
import scala.concurrent.duration.Duration
import scala.concurrent.duration.DurationInt
import scala.language.postfixOps
import Validation.ReflectionLib.extractSource.sourceCode


object StateEntity:
  def apply(name: String): StateEntity =
    val newState = new StateEntity(name)
    AgentEntity(newState)
    newState

def always: true = true

enum ConditionTypeEnum:
  case Always
  case Conditional
  case Failure


case class ConditionType(conditionType: ConditionTypeEnum ,cond: Option[() => Boolean] = None, cSource: Option[String] = None)

class ConditionConstraints(stateEntity: StateEntity, nextState: StateEntity, howManyMsgs: Int = 1, c: => Boolean = false):

  def getCondition: () => Boolean = () => c

  infix def `no condition triggers the switch`(cond: => true): FailureCondition =
    val newCond = new ConditionConstraints(stateEntity,nextState ,Int.MinValue, c)
//    stateEntity.conditions = newCond
    AgentEntity(stateEntity)
    AgentEntity(stateEntity, nextState, ConditionType(ConditionTypeEnum.Always,Some(() => true)))
    new FailureCondition(stateEntity)

  inline infix def when(inline cond: => Boolean): FailureCondition =
    val newCond = new ConditionConstraints(stateEntity, nextState,howManyMsgs, cond)
//    stateEntity.conditions = newCond
    val source = sourceCode(cond)
    AgentEntity(stateEntity)
//     inspect cond and see if its always then dont register here

//  TODO: clone jdk 17 see how they manage errors, strings in properties, use get property
    if source == sourceCode(always) then
        AgentEntity(stateEntity, nextState, ConditionType(ConditionTypeEnum.Always,Some(() => true), Some(source)))
    else AgentEntity(stateEntity, nextState, ConditionType(ConditionTypeEnum.Conditional,Some(() => cond), Some(source)))
    new FailureCondition(stateEntity)


class FailureCondition(stateEntity: StateEntity, fs: => Option[StateEntity] = None, d: => Duration = 0.seconds):
  
  def getFs: Option[StateEntity] = fs

//  TODO: Change logic for handling this
  infix def timeout(duration: scala.concurrent.duration.Duration): FailureCondition =
    val newFail = new FailureCondition(stateEntity, fs, duration)
    stateEntity.timeout = duration
    AgentEntity(stateEntity)
    newFail

  infix def fail2(failState: => StateEntity): FailureCondition =
    val newFail = new FailureCondition(stateEntity, Some(failState), d)
    AgentEntity(stateEntity)
    AgentEntity(stateEntity, failState, ConditionType(ConditionTypeEnum.Failure,Some(() => true)))
    newFail

  infix def orSwitch2(failState: => StateEntity): ConditionConstraints =
    AgentEntity(stateEntity)
    AgentEntity(stateEntity, failState)
    new ConditionConstraints(stateEntity, failState)


class StateEntity(
                   val name: String,
                   val behaviors: ListBuffer[BehaviorEntity] = ListBuffer(),
                   val onSwitchBehavior: Option[ProcessingContext ?=> Unit] = None,
                   val onSwitchCode: Option[String] = None,
                 ) extends DialsEntity:

  private val logger = CreateLogger(classOf[StateEntity])
  private var _timeout: Option[scala.concurrent.duration.Duration] = None

  override def toString: String =
    s"StateEntity($name, ${behaviors.map(_.toString).mkString})"

  def timeout: Option[scala.concurrent.duration.Duration] = _timeout

  def timeout_=(value: scala.concurrent.duration.Duration): Unit = _timeout = Some(value)
  
  infix def behaves(defBehavior: PartialFunction[Any, Unit]): StateEntity =
    AgentEntity.getCurrentAgentState match
      case Some(state) => state
      case None =>
        throw new IllegalStateException(s"The ent ${AgentEntity.getCurrentAgent} has no current state - impossible!")

  infix inline def onSwitch(using ctx: ProcessingContext)(inline onSwitchBehavior: ProcessingContext ?=> Unit): StateEntity =
    AgentEntity.getCurrentAgentState match
      case Some(state) =>
        val newState = new StateEntity(name, behaviors, Some(onSwitchBehavior), Some(sourceCode(onSwitchBehavior)))
        AgentEntity(newState)
        newState
      case None =>
        throw new IllegalStateException(s"State '$name' has no agent context")

  infix def periodic(timer: (Int, Int, Int)): Unit =
    if ConfigDb.`DIALS.General.debugMode` then logger.info(s"Making the state $name periodic behavior for the ent ${AgentEntity.getCurrentAgent}")
    AgentEntity(this, timer)

  infix def switch2(nextState: => StateEntity): ConditionConstraints =
    if ConfigDb.`DIALS.General.debugMode` then logger.info(s"Switching from state $name to state ${nextState.name} for the ent ${AgentEntity.getCurrentAgent}")
    AgentEntity(this, nextState)
    new ConditionConstraints(this, nextState )

//  TODO: manage this
  infix def switchOnTimeout(nextState: => StateEntity): FailureCondition =
    if ConfigDb.`DIALS.General.debugMode` then logger.info(s"Switching from state $name to state ${nextState.name} for the ent ${AgentEntity.getCurrentAgent} on timeout only")
    AgentEntity(this, nextState)
    val newCond = new ConditionConstraints(this, nextState, Int.MinValue)
//    conditions = newCond
    new FailureCondition(this)
