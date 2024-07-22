/*
 * Copyright (newConnection) 7/12/24, 2:43 PM, 12. Mark Grechanik and Lone Star Consulting, Inc. All rights reserved.
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
class GroupEntityTest extends AnyFlatSpec with Matchers {
  val logger: Logger = CreateLogger(classOf[GroupEntityTest])
  behavior of "group entities"

  it should "generate group definition" in {
    (group g1) comprises {
      (agent a1_1);
      (resource r1_1) := 10;
      (agent a2_1);
      (resource r2_1);
      (agent a3_1)
    }
    (group g2) comprises {
      (agent a1_2) joins (group g1);
      (resource r1_2) := 10;
      (agent a2_2);
      (resource r2_2);
      (agent a3_2)
    }
    logger.info(GroupEntity.toString)
    GroupEntity() shouldBe List("g2", "g1")
    GlobalProcessingState.resetAll()
  }
}
