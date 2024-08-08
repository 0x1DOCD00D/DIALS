/*
 * Copyright (newConnection) 7/6/24, 1:44 PM, 6. Mark Grechanik and Lone Star Consulting, Inc. All rights reserved.
 *
 *  Unless required by applicable law or agreed to in writing, software distributed under the License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *
 *  See the License for the specific language governing permissions and limitations under the License.
 */

package GenericDefinitions

import GenericDefinitions.ResourceEntity.{containerResourcesStack, logger}
import Pdfs.PdfStreamGenerator
import Utilz.{ConfigDb, Constants, CreateLogger}

import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import InputDataProcessor.{TypeInfo, *}

/*
* A resource can be defined within an ent or independent of any other entity. A composite resource is a collection of one or more resources.
* Essentially, a resource is a store for some data and the data can be of any type even though in simulators only numbers or their collections are used.
* If a resource is declared as (resource r) at the top level and not within any ent then it should be added to the list of top level resource.
* If a resource is declared within an ent, which is a top-level entity, then it should be added to the list of resources of the ent.
* */

class ResourceEntity private (val name: String, val fieldResources: ListBuffer[ResourceEntity] = ListBuffer(), var values: LazyList[Double] = LazyList.empty, val dialsObjects: ListBuffer[DialsEntity] = ListBuffer() ) extends DialsEntity:
  private val linearSequence: LazyList[Int] = LazyList.from(1)
  override def toString: String =
    s"resource $name" +
      (if values.isEmpty then " holds no values" else s" holds values") +
      (if fieldResources.isEmpty then " and it doesn't have any fields"
      else s" has fields ${fieldResources.map(_.name)}\n")

  def getValues: LazyList[Double] =
    if name == Constants.LinearSequence then linearSequence.map(_.toDouble)
    else if values.isEmpty then
      logger.error(s"Resource $name has no values")
      LazyList.empty
    else values

  infix def contains[T](resources: => T): ResourceEntity =
    if containerResourcesStack.isEmpty then
      GlobalProcessingState(this) match
        case Left(errMsg) =>
          logger.error(errMsg)
        case Right(value) =>
          logger.info(s"Setting the global processing state to $value")
    containerResourcesStack.push(this)
    resources
    containerResourcesStack.pop()
    if containerResourcesStack.isEmpty then GlobalProcessingState(NoEntity)
    this

  infix def :=[T <: DistributionEntity | AnyVal | DialsEntity](setV: T*)(using ti: TypeInfo[T]): Unit =
    if ConfigDb.`DIALS.General.debugMode` then logger.info(s"Setting the value of the resource $name to $setV")
    val (numbers, dialsObjs) = InputDataProcessor(setV*)
    if !numbers.isEmpty then values = numbers
    if !dialsObjs.isEmpty then dialsObjects.prependAll(dialsObjs)
    

object ResourceEntity:
  private val topLevelResources: ListBuffer[ResourceEntity] = ListBuffer()
  private var containerResourcesStack: mutable.Stack[ResourceEntity] = mutable.Stack[ResourceEntity]()
  private val logger = CreateLogger(classOf[ResourceEntity])

  override def toString: String = topLevelResources.map(_.toString).mkString("\n")

  def resetAll(): Unit =
    topLevelResources.clear()
    containerResourcesStack.clear()

  def findResource(ref: ResourceEntity): Option[ResourceEntity] =
    topLevelResources.find(_.name == ref.name) match
      case Some(r) => Some(r)
      case None => None

  def apply(): List[String] = topLevelResources.map(_.name).toList
  def apply(name: String): ResourceEntity =
    if ConfigDb.`DIALS.General.debugMode` then logger.info(s"Current global processing state is ${GlobalProcessingState.getCurrentProcessingState}")
    val newRes = new ResourceEntity(name)
    if GlobalProcessingState.isAgent then
      if containerResourcesStack.isEmpty then
        AgentEntity(newRes)
      else 
        containerResourcesStack.top.fieldResources += newRes
        newRes
    else if GlobalProcessingState.isResource then
      if containerResourcesStack.nonEmpty then
        containerResourcesStack.top.fieldResources += newRes
      newRes
    else if GlobalProcessingState.isGroup then
      GroupEntity(newRes)
      newRes
    else if GlobalProcessingState.isNoEntity then
      if ConfigDb.`DIALS.General.debugMode` then logger.info(s"Adding new global resource entity $name to the top level resources")
      topLevelResources.prependAll(List(newRes))
      newRes
    else
      throw new IllegalStateException(s"Resource $name is not defined within an ent or at the top level with the global state ${GlobalProcessingState.getCurrentProcessingState}")