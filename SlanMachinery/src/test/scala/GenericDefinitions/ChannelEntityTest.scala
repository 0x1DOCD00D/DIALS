/*
 * Copyright (c) 7/13/24, 2:27 PM, 13. Mark Grechanik and Lone Star Consulting, Inc. All rights reserved.
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
class ChannelEntityTest extends AnyFlatSpec with Matchers {
  val logger: Logger = CreateLogger(classOf[ChannelEntityTest])
  behavior of "message entities"

  it should "generate channel definitions without messages" in {
    (channel c1)
    (channel c2)
    (channel c3)
    (channel c3)
    (channel c4)
    (channel c5)
    logger.info(ChannelEntity.toString)
    ChannelEntity() shouldBe List("c5", "c4", "c3", "c3", "c2", "c1")
    GlobalProcessingState.resetAll()
  }

  it should "generate message definitions with messages" in {
    (channel c1) transports (Keywords.message m1)
    (channel c2) transports {
      (Keywords.message m1);
      (Keywords.message m2) triggers (action b1);
      (Keywords.message m2) triggers (action b2);
      (Keywords.message m3)
    }
    (channel c3) transports (Keywords.message m10)
    logger.info(ChannelEntity.toString)
    ChannelEntity() shouldBe List("c3", "c2", "c1")
    GlobalProcessingState.resetAll()
  }

  it should "generate message definitions with messages and behaviors" in {
    (channel c1) transports (Keywords.message m1)
    (channel c2) transports {
      (Keywords.message m1) triggers (action b1);
      (Keywords.message m2);
      (Keywords.message m3) triggers (action b2);
    }
    (channel c3) transports (Keywords.message m10)
    logger.info(ChannelEntity.toString)
    ChannelEntity() shouldBe List("c3", "c2", "c1")
    GlobalProcessingState.resetAll()
  }
}
