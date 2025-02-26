/*
 * Copyright (newConnection) 7/6/24, 1:39 PM, 6. Mark Grechanik and Lone Star Consulting, Inc. All rights reserved.
 *
 *  Unless required by applicable law or agreed to in writing, software distributed under the License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *
 *  See the License for the specific language governing permissions and limitations under the License.
 */

package GenericDefinitions

import GenericDefinitions.BehaviorEntity.{behaviors, logger}
import Utilz.Constants.EmptyBehaviorID
import Utilz.CreateLogger
import Validation.ReflectionLib.IdentInspector.inspect
import Validation.ReflectionLib.doesInspector.inspectDoesBlock

import scala.collection.mutable.ListBuffer

case object EmptyBehavior extends BehaviorEntity(EmptyBehaviorID)

class BehaviorEntity(val name: String, val triggerMsgs: ListBuffer[MessageEntity] = ListBuffer(), val actualActions: ListBuffer[PartialFunction[Any, Unit]] = ListBuffer(), val onActiveActions : ListBuffer[() => Unit] = ListBuffer(),val actualActionsCode: ListBuffer[String] = ListBuffer(), val onActiveActionsCode: ListBuffer[String] = ListBuffer()) extends DialsEntity:
  override def toString: String = s"$name " 
    + (if actualActions.nonEmpty then s"has ${actualActions.toList.length} actions" else "is empty")
    + (if triggerMsgs.nonEmpty then s" and is triggered by ${triggerMsgs.toList.length} messages" else " and it's triggered by all messages")
  
  def get: PartialFunction[Any, Unit] = actualActions.foldLeft(PartialFunction.empty)((a, b) => a.orElse(b))
  
  infix def triggeredBy(msgs: => MessageEntity): BehaviorEntity =
    if GlobalProcessingState.isNoEntity then
      GlobalProcessingState(this) match
        case Left(errMsg) =>
          logger.error(errMsg)
          throw new IllegalArgumentException(errMsg)
        case Right(_) =>
          msgs
          GlobalProcessingState(NoEntity)
          this
    else if GlobalProcessingState.isAgent then
      msgs
      this
    else throw new IllegalStateException(s"Behavior $name's message triggers $msgs cannot be defined within other entity ${GlobalProcessingState.getCurrentProcessingState}")

  inline infix def does(inline block: PartialFunction[Any, Unit]):PartialFunction[Any, Unit] =
    val res = inspectDoesBlock(block)
    this doesInternal (res._2().asInstanceOf[PartialFunction[Any, Unit]], res._1, res._3, res._4)


  private infix def doesInternal(defBehavior: PartialFunction[Any, Unit], onActiveFunc: () => Unit, onActiveCode: String, actualActionCode: String): PartialFunction[Any, Unit] =
    val nb = new BehaviorEntity(name, ListBuffer(), ListBuffer(defBehavior), ListBuffer(onActiveFunc), ListBuffer(actualActionCode), ListBuffer(onActiveCode))
    if GlobalProcessingState.isAgent then
      AgentEntity(nb)
      defBehavior
    else if GlobalProcessingState.isNoEntity then
      GlobalProcessingState(nb) match
        case Left(errMsg) =>
          logger.error(errMsg)
          throw new IllegalArgumentException(errMsg)
        case Right(_) =>
          behaviors.toList.find(_.name == name) match
            case Some(b) =>
              b.actualActions.append(defBehavior)
              b.onActiveActions.append(onActiveFunc)
              b.actualActionsCode.append(actualActionCode)
              b.onActiveActionsCode.append(onActiveCode)
              GlobalProcessingState(NoEntity)
              b.get
            case None =>
              behaviors.prependAll(List(nb))
              GlobalProcessingState(NoEntity)
              defBehavior
          defBehavior
    else throw new IllegalStateException(s"Behavior $name cannot be defined within other entity ${GlobalProcessingState.getCurrentProcessingState}")

object BehaviorEntity:
  private val behaviors: ListBuffer[BehaviorEntity] = ListBuffer()
  private val logger = CreateLogger(classOf[BehaviorEntity.type])

  override def toString: String = behaviors.map(_.toString).mkString("\n")

  def resetAll(): Unit = behaviors.clear()

  def apply(): List[String] = behaviors.map(_.name).toList
  
  def apply(msg: MessageEntity): Unit =
    if behaviors.isEmpty then
      logger.error(s"Behavior cannot be triggered by message $msg because there are no behaviors defined")
    else
      behaviors.head.triggerMsgs.append(msg)

  def apply(name: String): BehaviorEntity =
    if GlobalProcessingState.isAgent then
      val nb = new BehaviorEntity(name)  
      AgentEntity(nb)
      nb
    else if GlobalProcessingState.isNoEntity then
      behaviors.toList.find(_.name == name) match
        case Some(b) =>
          logger.info(s"Behavior $name is already defined globally")
          b
        case None => 
          val nb = new BehaviorEntity(name)
          behaviors.prependAll(List(nb))
          nb
    else if GlobalProcessingState.isChannel then
      new BehaviorEntity(name)
    else throw new IllegalStateException(s"Behavior $name cannot be defined within other entity ${GlobalProcessingState.getCurrentProcessingState}")