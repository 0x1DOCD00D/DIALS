/*
 * Copyright (newConnection) 7/5/24, 2:10 PM, 5. Mark Grechanik and Lone Star Consulting, Inc. All rights reserved.
 *
 *  Unless required by applicable law or agreed to in writing, software distributed under the License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *
 *  See the License for the specific language governing permissions and limitations under the License.
 */

package GenericDefinitions

import Utilz.{ConfigDb, CreateLogger}
import Validation.Visitors.{Visitor, VisitorState}
import org.slf4j.Logger

import scala.Dynamic
import scala.language.dynamics
import scala.language.postfixOps

trait DialsEntity {
  def accept[S <: VisitorState](visitor: Visitor[S]): S = visitor.visit(this)
}

case object NoEntity extends DialsEntity

class KeywordTemplate4String extends Dynamic {
  val logger: Logger = CreateLogger(classOf[KeywordTemplate4String])

  infix def selectDynamic(name: String): String = 
    if ConfigDb.`DIALS.General.debugMode` then logger.info(s"Creating an entity named $name")
    name
}

class KeywordTemplate[T <: DialsEntity](classType: Class[T]) extends Dynamic {
  val logger: Logger = CreateLogger(classOf[KeywordTemplate[T]])

  infix def selectDynamic(name: String): T = {
    if ConfigDb.`DIALS.General.debugMode` then logger.info(s"Creating an entity of ${classType.getName} named $name")
    if classType == classOf[AgentEntity] then AgentEntity(name).asInstanceOf[T]
    else if classType == classOf[ResourceEntity] then ResourceEntity(name).asInstanceOf[T]
    else if classType == classOf[ChannelEntity] then ChannelEntity(name).asInstanceOf[T]
    else if classType == classOf[GroupEntity] then GroupEntity(name).asInstanceOf[T]
    else if classType == classOf[MessageEntity] then MessageEntity(name).asInstanceOf[T]
    else if classType == classOf[ModelEntity] then ModelEntity(name).asInstanceOf[T]
    else if classType == classOf[FieldEntity] then FieldEntity(name).asInstanceOf[T]
    else if classType == classOf[StateEntity] then
      if AgentEntity.getState(name).isDefined then AgentEntity.getAndSetState(name).get.asInstanceOf[T]
      else StateEntity(name).asInstanceOf[T]
    else if classType == classOf[BehaviorEntity] then BehaviorEntity(name).asInstanceOf[T]
    else if classType == classOf[DistributionEntity] then DistributionEntity(name).asInstanceOf[T]
    else if classType == classOf[EntityInstanceAlias] then EntityInstanceAlias(name).asInstanceOf[T]
    else throw new IllegalArgumentException(s"Unknown entity type $classType")
  }
}
