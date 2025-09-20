/*
 * Copyright (c) 7/28/24, 11:16 AM, 28. Mark Grechanik and Lone Star Consulting, Inc. All rights reserved.
 *
 *  Unless required by applicable law or agreed to in writing, software distributed under the License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *
 *  See the License for the specific language governing permissions and limitations under the License.
 */

package GenericDefinitions.TestingDistributedAlternator

import GenericDefinitions._
import GenericDefinitions.Keywords._
import GenericDefinitions.PatternMatch4Messages._
import Utilz.CreateLogger
import Validation.DialsValidator
import Validation.Results.ValidationResult
import Validation.States.ValidationState
import org.scalatest.DoNotDiscover
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.slf4j.Logger
import scala.concurrent.duration.DurationInt
import scala.language.{dynamics, postfixOps}
import Utilz.Metrics._

@DoNotDiscover
class FullSimulationTestsOriginal extends AnyFlatSpec with Matchers {
  val logger: Logger = CreateLogger(classOf[FullSimulationTestsOriginal])
  behavior of "full simulations using DIALS"

  it should "generate a model definition for a distributed alternator simulation" in {
    measure("Compile Model and Validate") {
//      measure("Compile model"){
      (resource randomWait) := ((pdf UniformIntegerDistribution) as(5, 10))
      (resource LinearSequence) := 1
      (dispatch AskPermission)
      (dispatch NoWayMyTurn)
      (dispatch Goahead)
      (dispatch InformSinkProcess) := (pdf NormalDistribution) as(100, 10);

      (channel ControlAction) transports {
        (dispatch AskPermission)
        (dispatch NoWayMyTurn)
        (dispatch Goahead)
      }

      (channel Data) transports {
        (dispatch InformSinkProcess)
      };

      (agent MessageSinkProcess) has {
        (resource messageCount) := 0

        (state Wait4Messages) behaves {
          (action ReceiveMessage) does {
            onEventRule {
              (received InformSinkProcess) -> ((v, f) => (resource messageCount) := (resource messageCount).getValues.toList.head.toInt + 1)
            }
          }
        }
      } autotrigger (state Wait4Messages)

      (agent AlternatorProcess) has {
        (resource responseCount) := 0
        (resource sentNotification) := 0
        (resource numberOfNeighbors) := 2
        (resource LinearSequence)
        (resource AgentAttrs) contains {
          (resource ProcessID) := 1
        }

        (state randomWait) behaves {
          //when the random wait time is expired the switch occurs
          //if a neighbor contacts you with AskPermission then respond with Goahead
          (action Waiting) does {
            onEventRule {
              (received AskPermission) -> ((v, f) => (dispatch Goahead) respond SenderAgent)
            }
          }
        } switchOnTimeout (state ContactNeighbors) timeout (resource randomWait).getValues.take(1).toList.head.toInt.seconds

        (state ContactNeighbors) onSwitch {
          //send message AskPermission to your neighbors
          //set the sentNotification resource to true
          val msgAsk4Permission = (dispatch AskPermission2) := (resource ProcessID).getValues.toList.head.toInt
          val sent = msgAsk4Permission send (channel ControlAction)
          (resource sentNotification) := sent.toList.length;
        } switch2 (state Wait4ResponsesFromNeighbors) when (resource sentNotification).getValues.toList.head.toInt == 1

        (state Wait4ResponsesFromNeighbors) behaves {
          //when a response is received the resource responseCount is incremented
          //when the number of responses equals the number of neighbors, the state changes
          //or when the wait time is expired the switch occurs
          //respond to AskPermission with NoWayMyTurn

          (action ReceiveResponse) does {

            onEventRule {
              (received AskPermission) -> { (v, f) => (dispatch NoWayMyTurn) respond SenderAgent; }
            } orElse onEventRule {
              (received Goahead) -> ((v, f) => (resource responseCount) := (resource responseCount).getValues.toList.take(1).head.toInt + 1)
            } orElse onEventRule {
              (received NoWayMyTurn) -> ((v, f) =>
                val agentID = v.asInstanceOf[List[Double]].head.toInt;
                if (resource ProcessID).getValues.toList.head.toInt > agentID then
                  (resource responseCount) := (resource responseCount).getValues.toList.head.toInt + 1
                )
            }
          }
        } switch2 (state Proceed) when {
          (resource responseCount).getValues.toList.head.toInt == (resource numberOfNeighbors).getValues.toList.head.toInt
        } timeout 3.seconds fail2 (state randomWait);

        (state Proceed) onSwitch {
          (dispatch InformSinkProcess) send (channel Data);
          (resource sentNotification) := 1;
        } switch2 (state randomWait) when ((resource sentNotification).getValues.toList.head.toInt == 1) timeout 3.seconds
      }

      (model distributedAlternator) `is defined as` {
        |(agent AlternatorProcess) | := exactly (instance A);
        |(agent AlternatorProcess) | := exactly (instance B);
        |(agent AlternatorProcess) | := exactly (instance C);
        |(agent AlternatorProcess) | := exactly (instance D);
      } `is defined as` {
        (agent A) <~> (channel ControlAction) <~> (agent B) <~> (channel ControlAction) <~> (agent C) <~> (channel ControlAction) <~> (agent D) <~> (channel ControlAction) <~> (agent A);
        (agent A) ~> (channel Data) ~> (agent MessageSinkProcess);
        (agent B) ~> (channel Data) ~> (agent MessageSinkProcess);
        (agent C) ~> (channel Data) ~> (agent MessageSinkProcess);
        (agent D) ~> (channel Data) ~> (agent MessageSinkProcess);
      }
      //    logger.info(ModelEntity.toString)
      ModelEntity().map(_.name) shouldBe List("distributedAlternator")
//    }




        val st = ValidationState.empty

        val stNew = summon[DialsValidator[DialsEntity]].processIR(ModelEntity().head, st)

        logger.info(s"stNew = ${stNew.toString}")

        val res = ValidationResult()

        val res2 = summon[DialsValidator[DialsEntity]].validate(ModelEntity().head, stNew, res)
        val j = ResourceEntity
        logger.info(s"res2 = ${res2.toString}")


      ModelEntity.resetAll()

    }
  }
}


/*

 */