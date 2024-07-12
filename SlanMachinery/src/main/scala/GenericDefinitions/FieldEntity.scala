/*
 * Copyright (c) 7/10/24, 3:20 PM, 10. Mark Grechanik and Lone Star Consulting, Inc. All rights reserved.
 *  
 *  Unless required by applicable law or agreed to in writing, software distributed under the License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  
 *  See the License for the specific language governing permissions and limitations under the License.
 */

package GenericDefinitions

class FieldEntity(val name: String) extends DialsEntity:
  infix def :=[T](values: T*): FieldEntity = this
    
object FieldEntity:
  def apply(name: String): FieldEntity = new FieldEntity(name)