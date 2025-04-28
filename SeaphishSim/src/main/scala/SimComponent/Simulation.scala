package SimComponent
import scala.language.dynamics
import scala.language.postfixOps
import GenericDefinitions.*
import Keywords.*
import Validation.Results.ValidationResult
import Validation.DialsValidator
import Validation.States.ValidationState
import PatternMatch4Messages.*
object Simulation:

  def buildModel(numOfSEAPHISH: Int, numOfAttacker: Int): (ModelEntity, ValidationResult) = {
    /**
     * global groups, resources, messages and message channels
     */
    (group Attacker_group)
    (group SEAPHISH_group)

    (resource ListOfBankingAppOpened)
    (resource ListOfMaliciousApp)


    (dispatch Done)
    (dispatch SEAPHISH_Event)
    (dispatch Attacker_Event)

    (channel Sim_to_SEAPHISH) transports {
      (dispatch SEAPHISH_Event)
    }

    (channel Sim_to_Attacker) transports{
      (dispatch Attacker_Event)
    }
    (channel Attacker_to_Sim) transports{
      (dispatch Done)
    }
    (channel SEAPHISH_to_Sim) transports{
      (dispatch Done)
    }
    /**
     * Simulator agent
     */
    (agent Simulator) has {
      (resource EventQueue)
      (resource SimDuration)
      (resource SimTime)
      (resource DoneCounter)
      (state StartSimulation_InitState) onSwitch{
        // TODO Create New EventQueue
      } switch2(state SendEvent)
      (state SendEvent) onSwitch {
        // TODO process next event in EventQueue
        if (true) {
          (dispatch SEAPHISH_Event) send (channel Sim_to_SEAPHISH)
        }
        else{
          (dispatch Attacker_Event) send (channel Sim_to_Attacker)
        }
      } switch2(state Wait_for_done)
      (state Wait_for_done) behaves{
        (action ReceiveDone) does{
          onEventRule{
            (received Done) -> { (v, f) =>
              // TODO When Receive done, return back to SendEvent, updateDoneCounter

            }
          }
        }
        // TODO if time < SimDuration, fail to End_simulation_Kill_All_Actor
      } switch2(state SendEvent) when (true) fail2(state End_simulation_Kill_All_Actor)
      (state End_simulation_Kill_All_Actor) onSwitch{
      }
    } autotrigger (state StartSimulation_InitState)

    /**
     * Malicious App agents
     */
    (agent MaliciousApp) has{
      (resource MalAppID)
      (resource ExploredThreshold)
      (resource EvaluateAppFreq)
      (state Attacker_Move) behaves {
        (action WaitForAttackEvent) does{
          onEventRule{
            (received Attacker_Event) -> { (v, f) =>
              // TODO: compare MalAppID and act accordingly
            }
          }
        }
      } switch2(state Evaluate_All_Banking_App)
      (state Evaluate_All_Banking_App) onSwitch{
        // TODO: Evaluate all banking apps on the global resource and decide the attack plan
      } switch2(state Attack_Target_App)
      (state Attack_Target_App) onSwitch {
        // TODO Attack the target app accordingly by modifying the ListOfMaliciousApp global resource accordingly
        (dispatch Done) send (channel Attacker_to_Sim)
      }switch2(state Attacker_Move)
    } autotrigger(state Attacker_Move)

    /**
     * SEAPHISH Agents
     */

    (agent SEAPHISH) has{
      (resource SEAPHISH_ID)
      (resource DepthOfPhish)
      (resource PerturbedCoeff)
      (resource StartPerturbDepth)
      (resource Max_Move)
      (state SEAPHISH_Evaluate_Mal_App) behaves{
        (action WaitForSEAPHISHEvent) does {
          onEventRule{
            (received SEAPHISH_Event) -> { (v, f) =>
              // TODO: Evaluate all malicious app and open accordingly
            }
          }
        }
        // TODO: Open an exisiting app if no new phish needs to be created.
      } switch2(state Open_Bank_App) when(true) fail2(state Create_New_Phish)
      (state Create_New_Phish) onSwitch{
        // TODO: create new phish
      } switch2(state Open_Bank_App)
      (state Open_Bank_App) onSwitch{
        // TODO: open a bank app (NetGameSim)
      } switch2(state Banking_App_Make_Move)
      (state Banking_App_Make_Move)onSwitch {
        // TODO: Simulate the moves through a banking app using NetGameSim with the given parameters
      } switch2(state Save_App_SM_to_DB)
      (state Save_App_SM_to_DB) onSwitch{
        // TODO: Save the app's explored states to ListOfBankingAppOpened
        (dispatch Done) send (channel SEAPHISH_to_Sim)
      } switch2(state SEAPHISH_Evaluate_Mal_App)
    }autotrigger(state SEAPHISH_Evaluate_Mal_App)

    (model SimModel) `is defined as`{

      for (num <- 0 until (numOfSEAPHISH)) {
        val name = "SEAPHISH" + num.toString
        |(agent SEAPHISH) | := exactly (instance selectDynamic(name))
      }


      for (num <- 0 until(numOfAttacker)){
        val name = "MalApp" + num.toString
        |(agent MaliciousApp) | := exactly (instance selectDynamic(name))
      }

      |(agent Simulator)| :=  exactly(instance SimulatorActor)
    } `is defined as`{
      (group SEAPHISH_group) ~> (channel SEAPHISH_to_Sim) ~> (agent SimulatorActor)
      (group Attacker_group) ~> (channel Attacker_to_Sim) ~> (agent SimulatorActor)
      (agent SimulatorActor) ~> (channel Sim_to_SEAPHISH) ~> (group SEAPHISH_group)
      (agent SimulatorActor) ~> (channel Sim_to_Attacker) ~> (group Attacker_group)
    }

    val st = ValidationState.empty
    val stNew = summon[DialsValidator[DialsEntity]].processIR(ModelEntity().head, st)
    val res = ValidationResult()
    val res2 = summon[DialsValidator[DialsEntity]].validate(ModelEntity().head, stNew, res)

    println(s"Validation result: ${res2.toString}")

    (ModelEntity().head, res2)
  }

