/*
 * Copyright (c) 7/12/24, 8:51 PM, 12. Mark Grechanik and Lone Star Consulting, Inc. All rights reserved.
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

import scala.{Dynamic, List}
import scala.language.dynamics
import scala.language.postfixOps
import Keywords.*
import org.scalatest.DoNotDiscover

@DoNotDiscover
class MessageEntityTest extends AnyFlatSpec with Matchers {
  val logger: Logger = CreateLogger(classOf[MessageEntityTest])
  behavior of "message entities"

  it should "generate message definitions without fields" in {
    (Keywords.message m1) := (pdf UniformIntegerDistribution);
    (Keywords.message m2) := (20, 30)
    logger.info(MessageEntity.toString)
    MessageEntity() shouldBe List("m2", "m1")
    GlobalProcessingState.resetAll
  }

  it should "generate message definitions with fields" in {
    (Keywords.message m1) comprises {
      (field f1_1) := (10, 3.14);
      (field f1_2) := (pdf UniformIntegerDistribution);
    }
    (Keywords.message m2) comprises {
      (field f2_1);
      (field f2_2);
    }
    logger.info(MessageEntity.toString)
    MessageEntity() shouldBe List("m2", "m1")
    GlobalProcessingState.resetAll
  }

  it should "put a message into channels" in {
    ((Keywords.message m1) send ((channel c1), (channel c2), (channel c3))).map {
      case (m, c) => (m.name, c.map(_.name).mkString(", ")  )
    }.toList shouldBe List(("m1", "c1, c2, c3"))
    GlobalProcessingState.resetAll
  }


  }
