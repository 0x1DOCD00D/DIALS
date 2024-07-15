/*
 * Copyright (c) 7/6/24, 1:39 PM, 6. Mark Grechanik and Lone Star Consulting, Inc. All rights reserved.
 *
 *  Unless required by applicable law or agreed to in writing, software distributed under the License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *
 *  See the License for the specific language governing permissions and limitations under the License.
 */

package GenericDefinitions

import GenericDefinitions.BehaviorEntity.{behaviors, logger}
import Utilz.CreateLogger

import scala.collection.mutable.ListBuffer

case object EmptyBehavior extends BehaviorEntity("EmptyBehavior")

class BehaviorEntity(val name: String, val triggerMsgs: ListBuffer[MessageEntity] = ListBuffer(), val actualActions: ListBuffer[() => Unit] = ListBuffer()) extends DialsEntity:
  override def toString: String = s"$name " 
    + (if actualActions.nonEmpty then s"has ${actualActions.toList.length} actions" else "is empty")
    + (if triggerMsgs.nonEmpty then s" and is triggered by ${triggerMsgs.toList.length} messages" else " and it's triggered by all messages")
  
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
    
  infix def does(defBehavior: => Unit): BehaviorEntity =
    val nb = new BehaviorEntity(name, ListBuffer(), ListBuffer(() => defBehavior))
    if GlobalProcessingState.isAgent then
      AgentEntity(nb)
      this
    else if GlobalProcessingState.isNoEntity then
      GlobalProcessingState(nb) match
        case Left(errMsg) =>
          logger.error(errMsg)
          throw new IllegalArgumentException(errMsg)
        case Right(_) =>
          behaviors.toList.find(_.name == name) match
            case Some(b) =>
              b.actualActions.append(() => defBehavior)
              GlobalProcessingState(NoEntity)
              this
            case None =>
              behaviors.prependAll(List(nb))
              GlobalProcessingState(NoEntity)
              this
          this
    else throw new IllegalStateException(s"Behavior $name cannot be defined within other entity ${GlobalProcessingState.getCurrentProcessingState}")

object BehaviorEntity:
  private val behaviors: ListBuffer[BehaviorEntity] = ListBuffer()
  private val logger = CreateLogger(classOf[BehaviorEntity.type])

  override def toString: String = behaviors.map(_.toString).mkString("\n")

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
    else throw new IllegalStateException(s"Behavior $name cannot be defined within other entity ${GlobalProcessingState.getCurrentProcessingState}")