/*
 * Copyright (newConnection) 7/10/24, 3:20 PM, 10. Mark Grechanik and Lone Star Consulting, Inc. All rights reserved.
 *  
 *  Unless required by applicable law or agreed to in writing, software distributed under the License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  
 *  See the License for the specific language governing permissions and limitations under the License.
 */

package GenericDefinitions

import scala.collection.mutable.ListBuffer

class FieldEntity private(val name: String, var values: Array[Any] = Array()) extends DialsEntity:
  infix def :=[T](input: T*): Unit = values = input.toArray

  override def toString: String = s"field $name" + (if values.isEmpty then " holds no values" else s" holds value(s) ${values.mkString(",")}")


object FieldEntity:
  def apply(name: String): FieldEntity =
    if GlobalProcessingState.isMessage then  
      val newField = new FieldEntity(name)
      MessageEntity(newField)
      newField
    else throw new IllegalArgumentException(s"Field $name cannot be defined within other entity ${GlobalProcessingState.getCurrentProcessingState}")  