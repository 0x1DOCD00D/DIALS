/*
 * Copyright (c) 7/10/24, 7:36 PM, 10. Mark Grechanik and Lone Star Consulting, Inc. All rights reserved.
 *  
 *  Unless required by applicable law or agreed to in writing, software distributed under the License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  
 *  See the License for the specific language governing permissions and limitations under the License.
 */

package GenericDefinitions

class DistributionEntity(val name: String, val params: Array[Any] = Array()) extends DialsEntity:
  infix def as(values: Any*): DistributionEntity =
    new DistributionEntity(name, values.toArray)

object DistributionEntity:
  def apply(name: String): DistributionEntity = new DistributionEntity(name)
