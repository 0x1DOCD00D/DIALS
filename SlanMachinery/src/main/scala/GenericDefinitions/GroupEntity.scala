/*
 * Copyright (newConnection) 7/6/24, 1:39 PM, 6. Mark Grechanik and Lone Star Consulting, Inc. All rights reserved.
 *  
 *  Unless required by applicable law or agreed to in writing, software distributed under the License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  
 *  See the License for the specific language governing permissions and limitations under the License.
 */

package GenericDefinitions

import GenericDefinitions.GroupEntity.logger
import Utilz.CreateLogger

import scala.collection.mutable.ListBuffer

class GroupEntity private (name: String, agents: ListBuffer[AgentEntity] = ListBuffer(), resources: ListBuffer[ResourceEntity] = ListBuffer()) extends DialsEntity:
  override def toString: String = s"group $name\n" + agents.mkString("\n") + resources.mkString("\n")
  private def nameIs: String = name
  
  private def addAgent(a: AgentEntity): Unit = agents.appendAll(List(a))
  def removeAgent(a: AgentEntity): Unit = agents.filter(_.name == a.name).foreach(agents -= _)
  
  private def addResource(r: ResourceEntity): Unit = resources.appendAll(List(r))
  
  infix def comprises[T](members: => T): Unit =
    GlobalProcessingState(this) match
      case Left(errMsg) =>
        logger.error(errMsg)
      case Right(_) => 
        members
        GlobalProcessingState(NoEntity)

object GroupEntity:
  private val logger = CreateLogger(classOf[GroupEntity])
  private var groups: ListBuffer[GroupEntity] = ListBuffer()
  
  def resetAll(): Unit = groups.clear()
  
  def apply(name: String): GroupEntity = 
    val newG = new GroupEntity(name)
    groups.prependAll(List(newG)) 
    newG

  def apply(): List[String] = groups.map(_.nameIs).toList
  
  def apply(a: AgentEntity): Unit =
    require(a != null, "The agent cannot be null")
    groups.head.addAgent(a)

  def apply(r: ResourceEntity): Unit =
    require(r != null, "The resource cannot be null")
    groups.head.addResource(r)

  override def toString: String = groups.map(_.toString).mkString("\n")
  
  