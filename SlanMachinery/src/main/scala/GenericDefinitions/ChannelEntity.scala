/*
 * Copyright (newConnection) 7/6/24, 1:43 PM, 6. Mark Grechanik and Lone Star Consulting, Inc. All rights reserved.
 *
 *  Unless required by applicable law or agreed to in writing, software distributed under the License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *
 *  See the License for the specific language governing permissions and limitations under the License.
 */

package GenericDefinitions

import GenericDefinitions.ChannelEntity.logger
import Utilz.Constants.AllChannelsID
import Utilz.{ConfigDb, CreateLogger}

import scala.collection.mutable.ListBuffer

case object AllChannels extends ChannelEntity(AllChannelsID)

class ChannelEntity private[GenericDefinitions] (val name: String, val messages: ListBuffer[(MessageEntity, Option[BehaviorEntity])] = ListBuffer()) extends DialsEntity:
  override def toString: String =
    s"channel $name" +
      (if messages.isEmpty then " transports all types of messages"
      else
        s" can trasports only the messages of the following types: ${messages.map(_._1.name).mkString(", ")}\n" +
          messages.map { case (m, b) => s"Message ${m.name} triggers behavior ${b.map(_.name).getOrElse("None")}" }.mkString("\n")
        )

  infix def transports[T](messages: => T): Unit =
    if GlobalProcessingState.isNoEntity then
      GlobalProcessingState(this) match
        case Left(errMsg) =>
          logger.error(errMsg)
        case Right(value) =>
          logger.info(s"Setting the global processing state to $value")

          messages

          GlobalProcessingState(NoEntity) match
            case Left(errMsg) => logger.error(errMsg)
            case Right(_) => ()
    else logger.error(s"Message $name cannot be defined within other entity ${GlobalProcessingState.getCurrentProcessingState}")

object ChannelEntity:
  private val allChannels: ListBuffer[ChannelEntity] = ListBuffer()
  private val logger = CreateLogger(classOf[ChannelEntity])

  override def toString: String = allChannels.map(_.toString).mkString("\n")

  def resetAll(): Unit = allChannels.clear()

  def apply(): List[String] = allChannels.map(_.name).toList

  def apply(m: MessageEntity, b: Option[BehaviorEntity] = None): Unit =
    require(m != null, "Message cannot be null")
    if allChannels.isEmpty then throw new IllegalArgumentException("No channel entity is defined")
    else allChannels.head.messages.prependAll(List((m, b)))

  def apply(name: String): ChannelEntity =
    if ConfigDb.`DIALS.General.debugMode` then logger.info(s"New channel entity $name is created")
    val found = allChannels.find(_.name == name)
    if found.isDefined then
      if ConfigDb.`DIALS.General.debugMode` then logger.info(s"Channel entity $name already exists")
      val chn = found.get
      chn
    else
      val newC = new ChannelEntity(name)
      allChannels.prependAll(List(newC))
      newC
