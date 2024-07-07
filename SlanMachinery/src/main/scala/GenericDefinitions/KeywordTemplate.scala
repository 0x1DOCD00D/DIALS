/*
 * Copyright (c) 7/5/24, 2:10 PM, 5. Mark Grechanik and Lone Star Consulting, Inc. All rights reserved.
 *  
 *  Unless required by applicable law or agreed to in writing, software distributed under the License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  
 *  See the License for the specific language governing permissions and limitations under the License.
 */

package GenericDefinitions

import Utilz.{ConfigDb, CreateLogger}

import scala.Dynamic
import scala.language.dynamics
import scala.language.postfixOps

trait DialsEntity

class KeywordTemplate[T <: DialsEntity](classType: Class[T]) extends Dynamic {
  val logger = CreateLogger(classOf[KeywordTemplate[T]])
  
  infix def selectDynamic(name: String): T = {
    if ConfigDb.`DIALS.General.debugMode` then logger.info(s"Creating an entity of ${classType.getName} named $name")
    if classType == classOf[AgentEntity] then AgentEntity(name).asInstanceOf[T]
//    else if classType == classOf[ResourceEntity] then ResourceEntity(name)
//    else if classType == classOf[StateEntity] then StateEntity(name)
    else throw new IllegalArgumentException(s"Unknown entity type $classType")
  }
}
