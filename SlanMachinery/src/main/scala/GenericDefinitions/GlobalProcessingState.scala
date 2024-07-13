/*
 * Copyright (c) 7/10/24, 2:11 PM, 10. Mark Grechanik and Lone Star Consulting, Inc. All rights reserved.
 *  
 *  Unless required by applicable law or agreed to in writing, software distributed under the License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  
 *  See the License for the specific language governing permissions and limitations under the License.
 */

package GenericDefinitions

import Utilz.ConfigDb

object GlobalProcessingState:
  private var currentProcessingState: DialsEntity = NoEntity
  
  def isChannel: Boolean = currentProcessingState.isInstanceOf[ChannelEntity]
  def isGroup: Boolean = currentProcessingState.isInstanceOf[GroupEntity]
  def isAgent: Boolean = currentProcessingState.isInstanceOf[AgentEntity]
  def isMessage: Boolean = currentProcessingState.isInstanceOf[MessageEntity]
  def isResource: Boolean = currentProcessingState.isInstanceOf[ResourceEntity]
  def isNoEntity: Boolean = currentProcessingState == NoEntity
  def getCurrentProcessingState: String = currentProcessingState.getClass.getSimpleName

  def resetAll: Unit = 
    currentProcessingState = NoEntity
    AgentEntity.resetAll
    ResourceEntity.resetAll
    GroupEntity.resetAll
    MessageEntity.resetAll
    ChannelEntity.resetAll
    
  def apply(state: DialsEntity): Either[String, DialsEntity] =
    state match
      case a: ChannelEntity if currentProcessingState == NoEntity =>
        currentProcessingState = a
        Right(currentProcessingState)
      case a: MessageEntity if currentProcessingState == NoEntity =>
        currentProcessingState = a
        Right(currentProcessingState)
      case a: GroupEntity if currentProcessingState == NoEntity =>
        currentProcessingState = a
        Right(currentProcessingState)
      case a: AgentEntity if currentProcessingState == NoEntity =>
        currentProcessingState = a
        Right(currentProcessingState)
      case a: ResourceEntity if currentProcessingState.isInstanceOf[GroupEntity] =>
        Left(s"Resource ${a.name} cannot be defined inside a group")
      case a: ResourceEntity if currentProcessingState == NoEntity =>
        currentProcessingState = a
        Right(currentProcessingState)
      case a: ResourceEntity if currentProcessingState.isInstanceOf[AgentEntity] | currentProcessingState.isInstanceOf[ResourceEntity] =>
        Right(currentProcessingState)
      case NoEntity => 
        currentProcessingState = NoEntity
        Right(currentProcessingState)
      case _ => Left(s"Error in setting the processing state to ${state.getClass.getSimpleName} from ${currentProcessingState.getClass.getSimpleName}")
