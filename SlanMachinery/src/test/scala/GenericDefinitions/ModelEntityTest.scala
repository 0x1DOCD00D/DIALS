/*
 * Copyright (newConnection) 7/16/24, 9:22 AM, 16. Mark Grechanik and Lone Star Consulting, Inc. All rights reserved.
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
class ModelEntityTest extends AnyFlatSpec with Matchers {
  val logger: Logger = CreateLogger(classOf[GroupEntityTest])
  behavior of "model entities"

  it should "generate a model definition" in {
    (model m1) `is defined as` {
      (agent a1) <~> (channel c1) <~> (agent a2);
      (agent a2) ~> (channel c2) ~> (agent a3);
      (agent a3) <~ (channel c3) <~ (agent a1)
    }
    logger.info(ModelEntity.toString)
    ModelEntity() shouldBe List("m1")
    ModelEntity.resetAll()
  }
}
