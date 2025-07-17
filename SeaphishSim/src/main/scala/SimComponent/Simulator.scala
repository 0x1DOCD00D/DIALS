package SimComponent
import scala.language.dynamics
import scala.language.postfixOps
import GenericDefinitions.*
import Keywords.*
import Validation.Results.ValidationResult
import Validation.DialsValidator
import Validation.States.ValidationState
import PatternMatch4Messages.*
object Simulator:
  /**
   * Simulator with a simple SEAPHIHSH and a Attacker actor for testing and verification.
   */
  def buildSimulator() = {
    (dispatch EventAttack)
    (dispatch EventSEAPHISH)
    (dispatch AttackDone)
    (dispatch SEAPHISHDone)
    (channel Attacker_to_Sim) transports {
      (dispatch AttackDone)
    }
    (channel SEAPHISH_to_Sim) transports {
      (dispatch SEAPHISHDone)
    }
    (channel Sim_to_Attacker) transports {
      (dispatch EventAttack)
    }
    (channel Sim_to_SEAPHISH) transports {
      (dispatch EventSEAPHISH)
    }
    (agent Attacker) has {
      (resource Att)
      (state A) behaves {
        (action Ab) does {
          onEventRule {
            (received EventAttack) -> { (v, f) =>
              print("Attacker received")
              (dispatch AttackDone) send (channel Attacker_to_Sim)
            }
          }
        }
      }
    }


    (agent SEAPHISH) has {
      (resource SEA)
      (state S) behaves {
        (action Sb) does {
          onEventRule {
            (received EventSEAPHISH) -> { (v, f) =>
              print("SEAPHISH received")
              (dispatch SEAPHISHDone) send (channel SEAPHISH_to_Sim)
            }
          }
        }
      }
    }
    
    
    (agent Simulator) has {
      (resource EventQueue) := ((pdf UniformIntegerDistribution) as(0, 1))
      (resource SimDuration) := 5// total week per sim
      (resource SimTime) := 0// week number
      (resource DoneCounter) := 0
      (resource EventPerRound) := 5
      (resource CurrEventCount) := 0 // current event number
      // The last event number
      (resource EndLastRound) := (resource CurrEventCount).getValues.head.toInt + (resource EventPerRound).getValues.head.toInt
      (resource CurrEvent)
      (state StartSimulation_InitState) onSwitch {
        (resource CurrEvent) := (resource EventQueue).getValues.toList.apply((resource CurrEventCount).getValues.head.toInt).toInt
      } switch2 (state SendEvent)
      (state SendEvent) onSwitch {
        // If begin = end means that the round is over. Increment SimTime and set new end last round
        // Get the current event from the EventQueue
        (resource CurrEvent) := (resource EventQueue).getValues.toList.apply((resource CurrEventCount).getValues.head.toInt).toInt
        if ((resource CurrEvent).getValues.toList.head.toInt == 0) {
          // 0 is a SEAPHISH event
          (dispatch EventSEAPHISH) send (channel Sim_to_SEAPHISH)
        }
        else {
          (dispatch EventAttack) send (channel Sim_to_Attacker)
        }
        // If the current event count is equal to the number of the event this event round is supposed to end, update the sim time and the new event count for the next event 
        if ((resource CurrEventCount).getValues.head.toInt == (resource EndLastRound).getValues.head.toInt) {
          (resource SimTime) := (resource SimTime).getValues.head.toInt + 1
          (resource EndLastRound) := (resource CurrEventCount).getValues.head.toInt + (resource EventPerRound).getValues.head.toInt
        }
        (resource CurrEventCount) := (resource CurrEventCount).getValues.head.toInt + 1
      } switch2 (state Wait_for_done)
      (state Wait_for_done) behaves {
        (action ReceiveDone) does {
          onEventRule {
            (received AttackDone) -> { (v, f) => print("AttackDone received")
            }
          }orElse onEventRule{
            (received SEAPHISHDone) -> { (v, f) => print("SEAPHISHDone received")
            }
          }
        }
      } switch2 (state SendEvent) when{
        (resource SimTime).getValues.head.toInt < (resource SimDuration).getValues.head.toInt
      } fail2(state End_simulation_Kill_All_Actor);
      
      (state End_simulation_Kill_All_Actor) onSwitch {
      }
      
      
    } autotrigger (state StartSimulation_InitState)
    
    (model SimulatorModel) `is defined as`{
//      |(agent Simulator)| := exactly(instance Sim);
//      |(agent Attacker)| := exactly(instance A_Agent);
//      |(agent SEAPHISH)| := exactly(instance S_Agent);

    } `is defined as`{
//      (agent A_Agent) ~> (channel Attacker_to_Sim) ~> (agent Sim);
//      (agent S_Agent) ~> (channel SEAPHISH_to_Sim) ~> (agent Sim);
//      (agent Sim) ~> (channel Sim_to_Attacker) ~> (agent A_Agent);
//      (agent Sim) ~> (channel Sim_to_SEAPHISH) ~> (agent S_Agent);
        (agent Attacker) ~> (channel Attacker_to_Sim) ~> (agent Simulator);
        (agent SEAPHISH) ~> (channel SEAPHISH_to_Sim) ~> (agent Simulator);
        (agent Simulator) ~> (channel Sim_to_Attacker) ~> (agent Attacker);
        (agent Simulator) ~> (channel Sim_to_SEAPHISH) ~> (agent SEAPHISH);
    };
//    val st = ValidationState.empty
//    val stNew = summon[DialsValidator[DialsEntity]].processIR(ModelEntity().head, st)
//    val res = ValidationResult()
//    val res2 = summon[DialsValidator[DialsEntity]].validate(ModelEntity().head, stNew, res)
//
//    println(s"Validation result: ${res2.toString}")
//
//    (ModelEntity().head, res2)

    val st = ValidationState.empty

    val stNew = summon[DialsValidator[DialsEntity]].processIR(ModelEntity().head, st)
    
    val res = ValidationResult()

    val res2 = summon[DialsValidator[DialsEntity]].validate(ModelEntity().head, stNew, res)


    ModelEntity.resetAll()
  }
  
