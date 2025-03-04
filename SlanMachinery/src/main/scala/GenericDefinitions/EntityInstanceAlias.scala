/*
 * Copyright (c) 7/26/24, 2:03 PM, 26. Mark Grechanik and Lone Star Consulting, Inc. All rights reserved.
 *
 *  Unless required by applicable law or agreed to in writing, software distributed under the License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *
 *  See the License for the specific language governing permissions and limitations under the License.
 */

package GenericDefinitions

case class EntityInstanceAlias(val alias: String, val ent: Option[DialsEntity] = None) extends DialsEntity:
  def name: String = alias
  def copy(entity: DialsEntity): EntityInstanceAlias = EntityInstanceAlias(alias, Some(entity))
