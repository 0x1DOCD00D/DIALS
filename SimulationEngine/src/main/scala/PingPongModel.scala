
import GenericDefinitions.*
import GenericDefinitions.Keywords.*
import PatternMatch4Messages.*
import scala.language.dynamics
import scala.language.postfixOps
import scala.concurrent.duration.DurationInt
import Validation.DialsValidator
import Validation.Results.ValidationResult
import Validation.States.ValidationState

object PingPongModel:

  def buildModel(): (ModelEntity, ValidationResult) = {
    // Define messages
    (dispatch Ping)
    (dispatch Pong)

    // Define channel
    (channel PingPongChannel) transports {
      (dispatch Ping)
      (dispatch Pong)
    }

    // AgentA: Starts the ping
    (agent AgentA) has {
      (resource pingCount) := 0

      (state Start) onSwitch {
        (dispatch Ping) send (channel PingPongChannel)
        (resource pingCount) := 1
      } switch2 (state Waiting)

      (state Waiting) behaves {
        (action ReceivePong) does {
          onEventRule {
            (received Pong) -> { (v, f) =>
              println("AgentA received Pong")
              (dispatch Ping) send (channel PingPongChannel)
              (resource pingCount) := (resource pingCount).getValues.head.toInt + 1
            }
          }
        }
      }
    } autotrigger (state Start)

    // AgentB: Responds with Pong
    (agent AgentB) has {
      (resource pongCount) := 0

      (state Listen) behaves {
        (action ReceivePing) does {
          onEventRule {
            (received Ping) -> { (v, f) =>
              println("AgentB received Ping")
              (dispatch Pong) send (channel PingPongChannel)
              (resource pongCount) := (resource pongCount).getValues.head.toInt + 1
            }
          }
        }
      }
    } autotrigger (state Listen)

    // Model definition
    (model pingPongModel) `is defined as` {
      |(agent AgentA) | := exactly (instance A)
      |(agent AgentB) | := exactly (instance B)
    } `is defined as` {
      (agent A) <~> (channel PingPongChannel) <~> (agent B)
    }

    // Validation
    val st = ValidationState.empty
    val stNew = summon[DialsValidator[DialsEntity]].processIR(ModelEntity().head, st)
    val res = ValidationResult()
    val res2 = summon[DialsValidator[DialsEntity]].validate(ModelEntity().head, stNew, res)

    println(s"Validation result: ${res2.toString}")

    (ModelEntity().head, res2)
  }


