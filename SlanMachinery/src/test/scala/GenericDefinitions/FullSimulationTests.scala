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
    (resource randomWait) := ((pdf UniformIntegerDistribution) as(5, 10));
    (Keywords.message AskPermission);
    (Keywords.message NoWayMyTurn);
    (Keywords.message Goahead);
    (Keywords.message InformSinkProcess) := (pdf NormalDistribution) as (100, 10);

    (channel ControlAction) transports {
      (Keywords.message AskPermission);
      (Keywords.message NoWayMyTurn);
      (Keywords.message Goahead);
    };

    (channel Data) transports {
      (Keywords.message InformSinkProcess);
    };
    
    (agent MessageSinkProcess) has {
      (resource messageCount) := 0;

      (state Wait4Messages) behaves {
        (action ReceiveMessage) does {
          (resource messageCount) := (resource messageCount).getValues.head.toInt + 1
        }
      }  
    }  autotrigger (state Wait4Message);
    
    (agent AlternatorProcess) has {
      (resource responseCount) := 0;
      (resource sentNotification) := false;
      (resource numberOfNeighbors) := 2;
      (resource responses)
      
      (state randomWait) behaves {
        //when the random wait time is expired the switch occurs
        //if a neighbor contacts you with AskPermission then respond with Goahead
      } switch2 (state ContactNeighbors) when always timeout (resource randomWait).getValues.take(1).head.toInt.seconds;
      
      (state ContactNeighbors) behaves {
        //send message AskPermission to your neighbors
        //set the sentNotification resource to true
        //respond to AskPermission with NoWayMyTurn
        (action SendRequest);
      } switch2 (state Wait4ResponsesFromNeighbors)

      (state Wait4ResponsesFromNeighbors) behaves {
        //when a response is received the resource responseCount is incremented
        //when the number of responses equals the number of neighbors, the state changes
        //or when the wait time is expired the switch occurs
        //respond to AskPermission with NoWayMyTurn
        (action ReceiveResponse) does {
          (resource responses).storeValues((Keywords.message response))
          (resource responseCount) := (resource responseCount).getValues.head.toInt + 1
        }
      } switch2 (state How2Proceed) when {
        (resource responseCount).getValues.head.toInt == (resource numberOfNeighbors).getValues.head.toInt
      } timeout 3.seconds
    }

    (state How2Proceed) behaves {
      (action SendOrRetreat)
    } switch2 (state randomWait) when ((resource sentNotification).getValues.head.toInt == 1) timeout 3.seconds

    (model distributedAlternator) `is defined as` {
      |(agent AlternatorProcess)| := exactly (instance A);
      |(agent AlternatorProcess)| := exactly (instance B);
      |(agent AlternatorProcess)| := exactly (instance C);
      |(agent AlternatorProcess)| := exactly (instance D);
    } `is defined as` {
      (agent A) <~> (channel ControlAction) <~> (agent B) <~> (channel ControlAction) <~> (agent C) <~> (channel ControlAction) <~> (agent D) <~> (channel ControlAction) <~> (agent A);
      (agent A) ~> (channel Data) ~> (agent MessageSinkProcess);
      (agent B) ~> (channel Data) ~> (agent MessageSinkProcess);
      (agent C) ~> (channel Data) ~> (agent MessageSinkProcess);
      (agent D) ~> (channel Data) ~> (agent MessageSinkProcess);
    }
    logger.info(ModelEntity.toString)
    ModelEntity().map(_.name) shouldBe List("m1")
    ModelEntity.resetAll()
  }
}