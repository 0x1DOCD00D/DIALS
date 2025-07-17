package SimComponent
import scala.language.dynamics
import scala.language.postfixOps
import GenericDefinitions.*
import Keywords.*
import Validation.Results.ValidationResult
import Validation.DialsValidator
import Validation.States.ValidationState
import PatternMatch4Messages.*


object SEAPHISH:
  def buildSEAPHISH() = {
    (channel Sim_to_SEAPHISH) transports {
      (dispatch SEAPHISH_Event)
    }
    (channel SEAPHISH_To_Sim) transports {
      (dispatch SEAPHISHDone)
    }
    (agent Simulator) has {
      (resource Sim)
      (state SendSEAPHISHEvent) onSwitch {
        (dispatch EventSEAPHISH) send (channel Sim_to_SEAPHISH)
      } switch2 (state ListenAndSendAttackEvent)
      (state ListenAndSendAttackEvent) behaves {
        (action ListenToDone) does {
          onEventRule {
            (received SEAPHISHDone) -> { (v, f) =>
              (dispatch EventSEAPHISH) send (channel Sim_to_SEAPHISH)
            }
          }
        }
      } switch2 (state ListenAndSendAttackEvent)
    } autotrigger (state SendSEAPHISHEvent)
    (agent SEAPHISH) has {
      (resource SEAPHISH_ID)
      (resource DepthOfPhish) := ((pdf UniformIntegerDistribution) as (150,300)) //max depth of the phish when create a new phish
      (resource PerturbedCoeff) := ((pdf UniformRealDistribution) as (0,0.01)) //perturbed coefficient of each node/edge
      (resource StartPerturbDepth) := ((pdf UniformIntegerDistribution) as (20,100)) //when do phishes start having their edges/nodes perturbed
      (resource Max_Move) := ((pdf UniformIntegerDistribution) as (20,200)) //max move a turn can the user/seaphish make. 
      (state SEAPHISH_Evaluate_Mal_App) behaves {
        (action WaitForSEAPHISHEvent) does {
          onEventRule {
            (received SEAPHISH_Event) -> { (v, f) =>
              // TODO: Evaluate all malicious app and open accordingly
            }
          }
        }
        // TODO: Open an exisiting app if no new phish needs to be created.
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
        (dispatch Done) send (channel SEAPHISH_to_Sim)
      } switch2 (state SEAPHISH_Evaluate_Mal_App)
    } autotrigger (state SEAPHISH_Evaluate_Mal_App)

    (model SEAPHISHModel) `is defined as`{
      
    } `is defined as` {
      (agent SEAPHISH) ~> (channel SEAPHISH_to_Sim) ~> (agent Simulator)
      (agent Simulator) ~> (channel Sim_to_SEAPHISH) ~> (agent SEAPHISH)
    }
    val st = ValidationState.empty

    val stNew = summon[DialsValidator[DialsEntity]].processIR(ModelEntity().head, st)

    val res = ValidationResult()

    val res2 = summon[DialsValidator[DialsEntity]].validate(ModelEntity().head, stNew, res)


    ModelEntity.resetAll()
  }
