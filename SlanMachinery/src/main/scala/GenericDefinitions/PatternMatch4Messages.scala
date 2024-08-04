/*
 * Copyright (c) 8/3/24, 2:47 PM, 3. Mark Grechanik and Lone Star Consulting, Inc. All rights reserved.
 *
 *  Unless required by applicable law or agreed to in writing, software distributed under the License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *
 *  See the License for the specific language governing permissions and limitations under the License.
 */

package GenericDefinitions

case class GenericMessageTemplate(
                                   name: String,
                                   values: List[Double] = List(),
                                   fields: Option[List[GenericMessageTemplate]] = None
                                 )

object PatternMatch4Messages:
  def doMatch(map: (String, () => Unit)): PartialFunction[Any, Unit] = {
    case GenericMessageTemplate(name, v, f) if name == map._1 => map._2()
  }
