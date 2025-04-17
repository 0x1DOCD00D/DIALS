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
import AkkaMessages.*
import akka.actor.ActorSystem
import akka.util.Timeout
import akka.pattern.ask
import akka.actor.ActorRef

import scala.concurrent.{ExecutionContext, Future, Await}
import scala.concurrent.duration.DurationInt
/*
* A resource can be defined within an ent or independent of any other entity. A composite resource is a collection of one or more resources.
* Essentially, a resource is a store for some data and the data can be of any type even though in simulators only numbers or their collections are used.
* If a resource is declared as (resource r) at the top level and not within any ent then it should be added to the list of top level resource.
* If a resource is declared within an ent, which is a top-level entity, then it should be added to the list of resources of the ent.
* */

// data REC ::= ResourceNil | {value, [REC]}

class ResourceEntity private (val name: String, val fieldResources: ListBuffer[ResourceEntity] = ListBuffer(), var values: LazyList[Double] = LazyList.empty, val dialsObjects: ListBuffer[DialsEntity] = ListBuffer() ) extends DialsEntity:
  private val linearSequence: LazyList[Int] = LazyList.from(1)
  override def toString: String =
    s"resource $name" +
      (if values.isEmpty then " holds no values" else s" holds values") +
      (if fieldResources.isEmpty then " and it doesn't have any fields"
      else s" has fields ${fieldResources.map(_.name)}\n")

  def getValues: LazyList[Double] =
    given Timeout = Timeout(5.seconds)

    if Ctx.flag && Ctx.kind == "Agent" then
      Ctx.resources.get(name) match
        case Some(resActor) =>
          val fut = resActor ? ResourceGetMessage(Ctx.self, name)
          try
            Await.result(fut, 5.seconds) match {
              case ResourceGetResponse(_, _, v: LazyList[?]) =>
                println(s"[$name] got resource $name: $v")
                v.asInstanceOf[LazyList[Double]]
              case other =>
                logger.error(s"Unexpected response for $name: $other")
                LazyList.empty
            }
          catch
            case e: Exception =>
              logger.error(s"Timeout/err fetching $name: ${e.getMessage}")
              LazyList.empty
        case None =>
          logger.error(s"Resource actor for $name not found"); LazyList.empty
    else
      fallbackValues

  private def fallbackValues: LazyList[Double] =
    if ConfigDb.`DIALS.General.debugMode` then logger.info(s"Getting the values of the resource $name (local)")
    if name == Constants.LinearSequence then linearSequence.map(_.toDouble)
    else if values.isEmpty then
      logger.error(s"Resource $name has no values")
      LazyList.empty
    else values
  def collectAllFieldResources: ListBuffer[ResourceEntity] =
    val allResources = ListBuffer[ResourceEntity]()
    fieldResources.foreach { resource =>
      allResources.append(resource)
      allResources.appendAll(resource.collectAllFieldResources)
    }
    allResources

  infix def contains[T](resources: => T): ResourceEntity =
    val curGlobalProcessingState = GlobalProcessingState.getCurrentProcessingStateEntity
    if containerResourcesStack.isEmpty then
      GlobalProcessingState(this) match
        case Left(errMsg) =>
          logger.error(errMsg)
        case Right(value) =>
          logger.info(s"Setting the global processing state to $value")
    containerResourcesStack.push(this)
    resources
    containerResourcesStack.pop()
    if containerResourcesStack.isEmpty then GlobalProcessingState(curGlobalProcessingState)
    this

  infix def set(v: Any): Unit = v match {
    case value =>
      value match {
        case _: AnyVal | _: DialsEntity | _: DistributionEntity =>
          this.:=(value.asInstanceOf[AnyVal | DialsEntity | DistributionEntity])
        case _ =>
          println(s"[ResourceEntity.set] Unsupported value type for resource $name: $value")
      }
  }

  infix def :=[T <: DistributionEntity | AnyVal | DialsEntity](setV: T*)(using ti: TypeInfo[T]): Unit =
    if Ctx.flag && Ctx.kind == "Agent" then
      Ctx.resources.get(name) match
        case Some(resActor) =>
          logger.info(s"Setting resource $name to $setV (via actor)")
          resActor ! ResourceSetMessage(Ctx.self, name, setV)
        case None =>
          logger.error(s"Resource actor for $name not found")
    else
      // local assignment (model‑build phase or inside ResourceActor itself)
      if ConfigDb.`DIALS.General.debugMode` then
        logger.info(s"Setting resource $name locally to $setV")
      val (nums, objs) = InputDataProcessor(setV*)
      if nums.nonEmpty  then values       = nums
      if objs.nonEmpty  then dialsObjects.prependAll(objs)


object ResourceEntity:
  private val topLevelResources: ListBuffer[ResourceEntity] = ListBuffer()
  private val containerResourcesStack: mutable.Stack[ResourceEntity] = mutable.Stack[ResourceEntity]()
  private val logger = CreateLogger(classOf[ResourceEntity])
  
  def getTopLevelResources: ListBuffer[ResourceEntity] = topLevelResources

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
    topLevelResources.find(_.name == name) match
      case Some(existingResource) =>
        if GlobalProcessingState.isGroup then
          GroupEntity(existingResource)
        if GlobalProcessingState.isAgent then
          AgentEntity(existingResource)
        existingResource // Return existing resource if found
      case None =>
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
          if ConfigDb.`DIALS.General.debugMode` then
            logger.info(s"Adding new global resource entity $name to the top level resources")
          topLevelResources.prepend(newRes)
          newRes
        else
          throw new IllegalStateException(s"Resource $name is not defined within an entity or at the top level with the global state ${GlobalProcessingState.getCurrentProcessingState}")