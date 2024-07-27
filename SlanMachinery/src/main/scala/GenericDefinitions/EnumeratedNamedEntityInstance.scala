/*
 * Copyright (c) 7/27/24, 12:52 PM, 27. Mark Grechanik and Lone Star Consulting, Inc. All rights reserved.
 *  
 *  Unless required by applicable law or agreed to in writing, software distributed under the License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  
 *  See the License for the specific language governing permissions and limitations under the License.
 */

package GenericDefinitions

import Utilz.{ConfigDb, CreateLogger}

import scala.collection.mutable.ListBuffer

trait EnumeratedNamedEntityInstance:
  val logger = CreateLogger(classOf[EnumeratedNamedEntityInstance])
  val enumeratedAliases: ListBuffer[EntityInstanceAlias] = ListBuffer()

  def getAliases(): List[EntityInstanceAlias] = enumeratedAliases.toList

  def addInstance(alias: EntityInstanceAlias): Unit =
    if enumeratedAliases.toList.exists(e => e._1 == alias.alias) then
      logger.warn(s"Agent alias ${alias.alias}.name}")
    else if ConfigDb.`DIALS.General.debugMode` then 
      if alias.ent.isDefined then logger.info(s"Creating an entity alias ${alias.alias} for entity ${alias.ent.get}")
      else logger.error(s"Error creating an entity alias ${alias.alias} for an undefined entity.")
    enumeratedAliases.prependAll(List(alias))



