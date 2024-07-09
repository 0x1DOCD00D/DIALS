/*
 * Copyright (c) 7/6/24, 1:38 PM, 6. Mark Grechanik and Lone Star Consulting, Inc. All rights reserved.
 *  
 *  Unless required by applicable law or agreed to in writing, software distributed under the License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  
 *  See the License for the specific language governing permissions and limitations under the License.
 */

package GenericDefinitions

import GenericDefinitions.StateEntity.currentState
import Utilz.CreateLogger

import scala.collection.mutable.ListBuffer

object StateEntity:
  private var currentState: Option[StateEntity] = None
  
  def apply(name: String): StateEntity =
    val newState = new StateEntity(name)
    AgentEntity(newState)
    currentState = Some(newState)
    newState

class StateEntity(val name: String, val behaviors: List[() => BehaviorEntity] = List()) extends DialsEntity:
  private val logger = CreateLogger(classOf[StateEntity])

  infix def behaves(defBehavior: => BehaviorEntity): StateEntity =
    val nse = new StateEntity(name, (() => defBehavior) :: behaviors)
    AgentEntity(nse)
    nse
    

  infix def switch2[T](nextState: StateEntity): StateEntity =
    require(nextState != null)
    logger.info(s"Switching from state $name to state ${nextState.name} for the agent ${AgentEntity.getCurrentAgent}")
    AgentEntity(this, nextState)  
    this