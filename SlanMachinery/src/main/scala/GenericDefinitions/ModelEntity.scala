/*
 * Copyright (c) 7/15/24, 9:14 AM, 15. Mark Grechanik and Lone Star Consulting, Inc. All rights reserved.
 *  
 *  Unless required by applicable law or agreed to in writing, software distributed under the License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  
 *  See the License for the specific language governing permissions and limitations under the License.
 */

package GenericDefinitions

import scala.collection.mutable.ListBuffer

class ModelEntity(val name: String) extends DialsEntity:
  def f = ???
  
object ModelEntity:
  private val modelEntities: ListBuffer[ModelEntity] = ListBuffer.empty

  def apply(name: String): ModelEntity = 
    new ModelEntity(name)

  def apply(): List[String] = modelEntities.toList.map(_.name)

  def resetAll(): Unit = modelEntities.clear()
