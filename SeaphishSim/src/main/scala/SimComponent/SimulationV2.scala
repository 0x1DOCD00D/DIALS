package SimComponent

import scala.language.dynamics
import scala.language.postfixOps
import GenericDefinitions.*
import Keywords.*
import Validation.Results.ValidationResult
import Validation.DialsValidator
import Validation.States.ValidationState
import PatternMatch4Messages.*

/**
 * Second iteration of the Simulation. The simulator is fully implemented. The attacker and SEAPHISH agent are missing NetGraphSim as the "banking apps". Missing group features.
 */
object SimulationV2:
  def buildModel() : (ModelEntity, ValidationResult) = {
    // TODO: modify these resources to store NetGraphSim as "banking apps".
    (resource ListOfBankingAppOpened)
    (resource ListOfMaliciousApp)
    
    
    (dispatch EventSEAPHISH)
    (dispatch EventAttack)
    (dispatch SEAPHISHDone)
    (dispatch AttackerDone)

    (channel Attacker_to_Sim) transports {
      (dispatch AttackDone)
    }
    (channel SEAPHISH_to_Sim) transports {
      (dispatch SEAPHISHDone)
    }
    (channel Sim_to_Attacker) transports {
      (dispatch EventAttack) //TODO: Add more messages to send to attackers (i.e AppID,...)
    }
    (channel Sim_to_SEAPHISH) transports {
      (dispatch EventSEAPHISH)//TODO: Add more messages to send to SEAPHISHES (i.e AppID,...)
    }

    /**
     * Simulator agent
     */

    (agent Simulator) has {
      (resource EventQueue) := ((pdf UniformIntegerDistribution) as (0, 1))
      (resource SimDuration) := 5 // total week per sim
      (resource SimTime) := 0 // week number
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
          } orElse onEventRule {
            (received SEAPHISHDone) -> { (v, f) => print("SEAPHISHDone received")
            }
          }
        }
      } switch2 (state SendEvent) when {
        (resource SimTime).getValues.head.toInt < (resource SimDuration).getValues.head.toInt
      } fail2 (state End_simulation_Kill_All_Actor);

      (state End_simulation_Kill_All_Actor) onSwitch {
      }
    } autotrigger (state StartSimulation_InitState)

    /**
     * Malicious App agents
     */
    
    (agent MalApp) has {
      (resource MalAppID)
      (resource ExploredThreshold) := ((pdf UniformRealDistribution) as (0, 1))
      (resource EvaluateAppFreq) := ((pdf UniformRealDistribution) as (0, 1))
      
      (state Attacker_Move) behaves {
        (action WaitForAttackEvent) does {
          onEventRule {
            (received EventAttack) -> { (v, f) =>
              // Do I need to process data in here if I just want to do a state transition based on the AppID?
              // Add another condition if needed if decide to add "provide service" function to the attacker. 
            }
          }
        }
      } switch2 (state Evaluate_All_Banking_App) when ((resource MalAppID).getValues.head.toInt == ((dispatch EventAttack).fields.head).asInstanceOf[Int]) fail2 (state Attacker_Move)
      // Switch to the next state only when the MalAppID matches the ones from the Simulator's message.
      (state Evaluate_All_Banking_App) onSwitch {
        // TODO: Evaluate all banking apps on the global resource and decide the attack plan. Do when NetGraphSim for SEAPHISH is done
      } switch2 (state Attack_Target_App)
      (state Attack_Target_App) onSwitch {
        // TODO Attack the target app accordingly by modifying the ListOfMaliciousApp global resource accordingly. Do when NetGraphSim for SEAPHISH is done.
        (dispatch AttackDone) send (channel Attacker_to_Sim)
      } switch2 (state Attacker_Move)
    } autotrigger (state Attacker_Move)

    /**
     * SEAPHISH Agent
     */
    
    (agent SEAPHISH) has {
      (resource SEAPHISH_ID)
      (resource DepthOfPhish) := ((pdf UniformIntegerDistribution) as (150, 300)) //max depth of the phish when create a new phish
      (resource PerturbedCoeff) := ((pdf UniformRealDistribution) as (0, 0.01)) //perturbed coefficient of each node/edge
      (resource StartPerturbDepth) := ((pdf UniformIntegerDistribution) as (20, 100)) //when do phishes start having their edges/nodes perturbed
      (resource Max_Move) := ((pdf UniformIntegerDistribution) as (20, 200)) //max move a turn can the user/seaphish make.
      (state SEAPHISH_Evaluate_Mal_App) behaves {
        (action WaitForSEAPHISHEvent) does {
          onEventRule {
            (received EventSEAPHISH) -> { (v, f) =>
              // TODO: Evaluate all malicious app and open accordingly
            }
          }
        }
        // TODO: Open an existing app if no new phish needs to be created.
      } switch2 (state Open_Bank_App) when (true) fail2 (state Create_New_Phish) // TODO: Replace "when (true)" with a condition when no new phish needs to be created
      (state Create_New_Phish) onSwitch {
        // TODO: create new phish
      } switch2 (state Open_Bank_App)
      (state Open_Bank_App) onSwitch {
        // TODO: open a bank app (NetGameSim)
      } switch2 (state Banking_App_Make_Move)
      (state Banking_App_Make_Move) onSwitch {
        // TODO: Simulate the moves through a banking app using NetGameSim with the given parameters
      } switch2 (state Save_App_SM_to_DB)
      (state Save_App_SM_to_DB) onSwitch {
        // TODO: Save the app's explored states to ListOfBankingAppOpened
        (dispatch SEAPHISHDone) send (channel SEAPHISH_to_Sim)
      } switch2 (state SEAPHISH_Evaluate_Mal_App)
    } autotrigger (state SEAPHISH_Evaluate_Mal_App)


    (model SimulatorModel) `is defined as` {
    } `is defined as` {
      (agent MalApp) ~> (channel Attacker_to_Sim) ~> (agent Simulator);
      (agent SEAPHISH) ~> (channel SEAPHISH_to_Sim) ~> (agent Simulator);
      (agent Simulator) ~> (channel Sim_to_Attacker) ~> (agent MalApp);
      (agent Simulator) ~> (channel Sim_to_SEAPHISH) ~> (agent SEAPHISH);
    };
    
    
    val st = ValidationState.empty
    val stNew = summon[DialsValidator[DialsEntity]].processIR(ModelEntity().head, st)
    val res = ValidationResult()
    val res2 = summon[DialsValidator[DialsEntity]].validate(ModelEntity().head, stNew, res)

    println(s"Validation result: ${res2.toString}")

    (ModelEntity().head, res2)
  }
