/*
 * Copyright (c) 7/6/24, 1:39 PM, 6. Mark Grechanik and Lone Star Consulting, Inc. All rights reserved.
 *
 *  Unless required by applicable law or agreed to in writing, software distributed under the License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *
 *  See the License for the specific language governing permissions and limitations under the License.
 */

package GenericDefinitions

import GenericDefinitions.BehaviorEntity.logger
import Utilz.CreateLogger

import scala.collection.mutable.ListBuffer

case object EmptyBehavior extends BehaviorEntity("EmptyBehavior")

class BehaviorEntity(val name: String, val actualAction: Option[() => Unit] = None) extends DialsEntity:
  override def toString: String = s"$name is " + (if actualAction.isDefined then "defined" else "empty")

//  infix def triggeredBy(messages: => ): BehaviorEntity =
  infix def does(defBehavior: => Unit): BehaviorEntity =
    val nb = new BehaviorEntity(name, Some(() => defBehavior))
    AgentEntity(nb)
    nb

object BehaviorEntity:
  private val behaviors: ListBuffer[BehaviorEntity] = ListBuffer()
  private val logger = CreateLogger(classOf[BehaviorEntity])
  def apply(name: String): BehaviorEntity =
    val nb = new BehaviorEntity(name)
    if GlobalProcessingState.isAgent then AgentEntity(nb)
    nb
