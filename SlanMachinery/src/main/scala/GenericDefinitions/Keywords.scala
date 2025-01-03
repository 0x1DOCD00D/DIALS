/*
 * Copyright (newConnection) 7/7/24, 12:35 PM, 7. Mark Grechanik and Lone Star Consulting, Inc. All rights reserved.
 *  
 *  Unless required by applicable law or agreed to in writing, software distributed under the License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  
 *  See the License for the specific language governing permissions and limitations under the License.
 */

package GenericDefinitions

object Keywords:
  val group = new KeywordTemplate(classOf[GroupEntity])
  val agent = new KeywordTemplate(classOf[AgentEntity])
  val state = new KeywordTemplate(classOf[StateEntity])
  val behavior = new KeywordTemplate(classOf[BehaviorEntity])
  val channel = new KeywordTemplate(classOf[ChannelEntity])
  val action = new KeywordTemplate(classOf[BehaviorEntity])
  val resource = new KeywordTemplate(classOf[ResourceEntity])
  val message = new KeywordTemplate(classOf[MessageEntity])
  val dispatch = new KeywordTemplate(classOf[MessageEntity])
  val field = new KeywordTemplate(classOf[FieldEntity])
  val model = new KeywordTemplate(classOf[ModelEntity])
  val pdf = new KeywordTemplate(classOf[DistributionEntity])
  val instance = new KeywordTemplate(classOf[EntityInstanceAlias])
  val received = new KeywordTemplate4String
