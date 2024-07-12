/*
 * Copyright (c) 7/11/24, 7:01 PM, 11. Mark Grechanik and Lone Star Consulting, Inc. All rights reserved.
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
class ResourceEntityTest extends AnyFlatSpec with Matchers {
  val logger: Logger = CreateLogger(classOf[AgentEntityTest])
  behavior of "resource entities"

  it should "generate resource definitions" in {
    (resource res1) := (10, 20, 3.14)
    (resource res2) contains {
      (resource contained1) := (10, 20, 3.14);
      (resource contained2) contains {
        (resource contained3) := (pdf NormalDistribution) as (10, 20)
      } := 20;
    }
    logger.info(ResourceEntity.toString)
    ResourceEntity() shouldBe List("res1", "res2")
  }
}
