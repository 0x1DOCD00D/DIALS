/*
 * Copyright (c) 7/3/24, 9:22 AM, 3. Mark Grechanik and Lone Star Consulting, Inc. All rights reserved.
 *  
 *  Unless required by applicable law or agreed to in writing, software distributed under the License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  
 *  See the License for the specific language governing permissions and limitations under the License.
 */

package Utilz

object CheckIfStringIsNumber:
  import scala.util.Try
  def isShort(p: String): Boolean = Try(p.toLong).isSuccess
  def isInt(p: String): Boolean = Try(p.toInt).isSuccess
  def isLong(p: String): Boolean = Try(p.toLong).isSuccess
  def isDouble(p: String): Boolean = Try(p.toDouble).isSuccess
  def isFloat(p: String): Boolean = Try(p.toFloat).isSuccess
  extension (x: String)
    def IsNumber: Boolean = isShort(x) || isInt(x) || isLong(x) || isDouble(x) || isFloat(x)
