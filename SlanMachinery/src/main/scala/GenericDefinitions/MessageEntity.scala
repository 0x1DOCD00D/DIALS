/*
 * Copyright (newConnection) 7/6/24, 1:43 PM, 6. Mark Grechanik and Lone Star Consulting, Inc. All rights reserved.
 *
 *  Unless required by applicable law or agreed to in writing, software distributed under the License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *
 *  See the License for the specific language governing permissions and limitations under the License.
 */

package GenericDefinitions

import GenericDefinitions.MessageEntity.logger
import Utilz.{ConfigDb, CreateLogger}

import scala.collection.mutable.ListBuffer

class MessageEntity private(val name: String, val fields: ListBuffer[FieldEntity] = ListBuffer(), var values: Array[Any] = Array()) extends DialsEntity:
  override def toString: String =
    s"message $name" +
      (if values.isEmpty then " holds no values" else s" holds value(s) ${values.mkString(",")}") +
      (if fields.isEmpty then " and it doesn't have any fields"
      else
        s" has fields ${fields.map(_.name)}\n" +
          fields.map(_.toString).mkString("\n")
        )

  infix def triggers[T](behavior: BehaviorEntity): MessageEntity =
    if GlobalProcessingState.isChannel then
      ChannelEntity(this, Some(behavior))
    else
      logger.error(s"Message $name can be used to specify triggering behavior only in channels instead of ${GlobalProcessingState.getCurrentProcessingState}")
    this

  infix def comprises[T](fields: => T): Unit =
    if GlobalProcessingState.isNoEntity then
      GlobalProcessingState(this) match
        case Left(errMsg) =>
          logger.error(errMsg)
        case Right(value) =>
          logger.info(s"Setting the global processing state to $value")

          fields

          GlobalProcessingState(NoEntity) match
            case Left(errMsg) => logger.error(errMsg)
            case Right(_) => ()
    else logger.error(s"Message $name cannot be defined within other entity ${GlobalProcessingState.getCurrentProcessingState}")

  infix def :=[T](setV: T*): Unit =
    logger.info(s"Setting the value of the resource $name to $setV")
    values = setV.toArray

  //TODO: need to implement the logic of the resource value retrieval
  def getStoredValues: Array[Any] = values //need to look up the global table of resources

  infix def send(channel: ChannelEntity*): Map[MessageEntity, Array[ChannelEntity]] = Map(this -> channel.toArray)

object MessageEntity:
  private val allMessages: ListBuffer[MessageEntity] = ListBuffer()
  private val logger = CreateLogger(classOf[MessageEntity])

  override def toString: String = allMessages.map(_.toString).mkString("\n")

  def resetAll(): Unit = allMessages.clear()

  def apply(): List[String] = allMessages.map(_.name).toList

  def apply(field: FieldEntity): Unit =
    require(field != null, "Field cannot be null")
    if allMessages.isEmpty then throw new IllegalArgumentException("No message entity is defined")
    else allMessages.head.fields.prependAll(List(field))

  def apply(name: String): MessageEntity =
    if ConfigDb.`DIALS.General.debugMode` then logger.info(s"New message entity $name is created")
    val newMsg = new MessageEntity(name)
    if GlobalProcessingState.isChannel then
      ChannelEntity(newMsg)
    if GlobalProcessingState.isAgent then
      AgentEntity(newMsg)
    if GlobalProcessingState.isBehavior then
      BehaviorEntity(newMsg)
    else allMessages.prependAll(List(newMsg))
    newMsg
