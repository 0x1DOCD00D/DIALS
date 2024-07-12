/*
 * Copyright (c) 7/6/24, 1:44 PM, 6. Mark Grechanik and Lone Star Consulting, Inc. All rights reserved.
 *
 *  Unless required by applicable law or agreed to in writing, software distributed under the License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *
 *  See the License for the specific language governing permissions and limitations under the License.
 */

package GenericDefinitions

import GenericDefinitions.ResourceEntity.{containerResourcesStack, logger}
import Utilz.{ConfigDb, CreateLogger}

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

/*
* A resource can be defined within an agent or independent of any other entity. A composite resource is a collection of one or more resources.
* Essentially, a resource is a store for some data and the data can be of any type even though in simulators only numbers or their collections are used.
* If a resource is declared as (resource r) at the top level and not within any agent then it should be added to the list of top level resource.
* If a resource is declared within an agent, which is a top-level entity, then it should be added to the list of resources of the agent.
* */

class ResourceEntity private (val name: String, val fieldResources: ListBuffer[ResourceEntity] = ListBuffer(), var values: Array[Any] = Array()) extends DialsEntity:
  override def toString: String = 
    s"resource $name" +
      (if values.isEmpty then " holds no values" else s" holds value(s) ${values.mkString(",")}") +
      (if fieldResources.isEmpty then " and it doesn't have any fields" 
      else 
        s" has fields ${fieldResources.map(_.name)}\n" +
        fieldResources.map(_.toString).mkString("\n")
        )

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

  infix def :=[T](setV: T*): Unit =
    logger.info(s"Setting the value of the resource $name to $setV")
    values = setV.toArray
  
  //TODO: need to implement the logic of the resource value retrieval
  def getStoredValues: Array[Any] = values //need to look up the global table of resources   

object ResourceEntity:
  private val topLevelResources: ListBuffer[ResourceEntity] = ListBuffer()
  private var containerResourcesStack: mutable.Stack[ResourceEntity] = mutable.Stack[ResourceEntity]()
  private val logger = CreateLogger(classOf[ResourceEntity])

  override def toString: String = topLevelResources.map(_.toString).mkString("\n")

  def findResource(ref: ResourceEntity): Option[ResourceEntity] =
    
    topLevelResources.find(_.name == ref.name) match
      case Some(r) => Some(r)
      case None => None

  def apply(): List[String] = topLevelResources.map(_.name).toList
  def apply(name: String): ResourceEntity =
    if ConfigDb.`DIALS.General.debugMode` then logger.info(s"Current global processing state is ${GlobalProcessingState.getCurrentProcessingState}")
    val newRes = new ResourceEntity(name)
    if GlobalProcessingState.isAgent && containerResourcesStack.isEmpty then
      AgentEntity(newRes)
      newRes
    else if GlobalProcessingState.isResource then
      if !containerResourcesStack.isEmpty then
        containerResourcesStack.top.fieldResources += newRes
      newRes
    else if GlobalProcessingState.isNoEntity then
      topLevelResources += newRes
      newRes
    else
      throw new IllegalStateException(s"Resource $name is not defined within an agent or at the top level with the global state ${GlobalProcessingState.getCurrentProcessingState}")