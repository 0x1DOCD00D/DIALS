/*
 * Copyright (c) 7/28/24, 11:16 AM, 28. Mark Grechanik and Lone Star Consulting, Inc. All rights reserved.
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
import scala.concurrent.duration.DurationInt

@DoNotDiscover
class FullSimulationTests extends AnyFlatSpec with Matchers {
  val logger: Logger = CreateLogger(classOf[FullSimulationTests])
  behavior of "full simulations using DIALS"

  it should "generate a model definition for a distributed alternator simulation" in {
    (agent AlternatorProcess) has {
      (resource responseCount) := 0;
      (resource numberOfNeighbors) := 2;

      (state randomWait) behaves {
      } switch2 (state ContactNeighbors) when true timeout 5.seconds;

      (state ContactNeighbors) behaves {
        (action SendRequest);
      } switch2 (state Wait4ResponsesFromNeighbors)

      (state Wait4ResponsesFromNeighbors) behaves {
        //when a response is received the resource responseCount is incremented
        //when the number of responses equals the number of neighbors, the state changes
        //or when the wait time is expired the switch occurs
        (action ReceiveResponse) does {
          (resource responseCount) := (resource responseCount).getValues.head.toInt + 1
        }
      } switch2 (state Wait4ResponsesFromNeighbors) when {
        (resource responseCount).getValues.head.toInt == (resource numberOfNeighbors).getValues.head.toInt
      } timeout 3.seconds
    }

    (model distributedAlternator) `is defined as` {
      (agent a1) <~> (channel c1) <~> (agent a2);
      (agent a2) ~> (channel c2) ~> (agent a3);
      (agent a3) <~ (channel c3) <~ (agent a1)
    }
    logger.info(ModelEntity.toString)
    ModelEntity().map(_.name) shouldBe List("m1")
    ModelEntity.resetAll()
  }
}