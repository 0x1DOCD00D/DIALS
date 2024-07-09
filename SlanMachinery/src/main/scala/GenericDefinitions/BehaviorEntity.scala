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

class BehaviorEntity(val name: String, val actualAction: Option[() => Unit] = None) extends DialsEntity:
  infix def switch2(nextState: StateEntity): BehaviorEntity =
    require(nextState != null)
    val currAgentState = AgentEntity.getCurrentAgentState
    if currAgentState.isDefined then
      logger.info(s"Switching from state ${currAgentState.get.name} to the state ${nextState.name} for the agent ${AgentEntity.getCurrentAgent}")
      AgentEntity(currAgentState.get, nextState)
      this
    else
      logger.error(s"The agent ${AgentEntity.getCurrentAgent} has no current state")
      throw new IllegalStateException(s"The agent ${AgentEntity.getCurrentAgent} has no current state")

  infix def contains(defBehavior: => Unit): BehaviorEntity =
    new BehaviorEntity(name, Some(() => defBehavior))

object BehaviorEntity:
  private val logger = CreateLogger(classOf[BehaviorEntity])
  def apply(name: String): BehaviorEntity = new BehaviorEntity(name)