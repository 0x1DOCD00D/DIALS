/*
 * Copyright (c) 7/14/24, 2:56 PM, 14. Mark Grechanik and Lone Star Consulting, Inc. All rights reserved.
 *  
 *  Unless required by applicable law or agreed to in writing, software distributed under the License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  
 *  See the License for the specific language governing permissions and limitations under the License.
 */

package GenericDefinitions

import org.scalatest.funsuite.AnyFunSuiteLike
import Utilz.{Constants, CreateLogger}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.slf4j.Logger

import scala.Dynamic
import scala.language.dynamics
import scala.language.postfixOps
import Keywords.*
import org.scalatest.DoNotDiscover

@DoNotDiscover
class BehaviorEntityTest extends AnyFlatSpec with Matchers {
  val logger: Logger = CreateLogger(classOf[AgentEntityTest])
  behavior of "action entities"

  it should "generate one behavior definition with multiple actions and messages" in {
    (action b5) does {
        println("b5 in s2")
      } does {
        val c = 2
      } does { } triggeredBy  {
        (Keywords.message m1)
        (Keywords.message m2)
      } triggeredBy {
        (Keywords.message m3)
        (Keywords.message m4)
        (Keywords.message m5)
      }

    logger.info(BehaviorEntity.toString)
    BehaviorEntity().length shouldBe 1
    GlobalProcessingState.resetAll
  }

}
