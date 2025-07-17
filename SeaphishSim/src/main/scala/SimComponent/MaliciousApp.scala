package SimComponent
import scala.language.dynamics
import scala.language.postfixOps
import GenericDefinitions.*
import Keywords.*
import Validation.Results.ValidationResult
import Validation.DialsValidator
import Validation.States.ValidationState
import PatternMatch4Messages.*


object MaliciousApp:
  def buildMalApp() = {
    (channel Attacker_to_Sim) transports {
      (dispatch AttackDone)
    }
    (channel Sim_to_Attacker) transports {
      (dispatch EventAttack) // TODO: add APPID to indicate which app to send the event to 
    }
    (agent Simulator) has {
      (resource Sim)
      (state SendAttackEvent) onSwitch {
        (dispatch EventAttack) send (channel Sim_to_Attacker)
      } switch2 (state ListenAndSendAttackEvent)
      (state ListenAndSendAttackEvent) behaves {
        (action ListenToDone) does {
          onEventRule {
            (received AttackDone) -> { (v, f) =>
              (dispatch EventAttack) send (channel Sim_to_Attacker)
            }
          }
        }
      } switch2 (state ListenAndSendAttackEvent)
    } autotrigger (state SendAttackEvent)
    (agent MalApp) has {
      (resource MalAppID)
      (resource ExploredThreshold) := ((pdf UniformRealDistribution) as (0,1))
      (resource EvaluateAppFreq) := ((pdf UniformRealDistribution) as (0,1)) 
      

      (state Attacker_Move) behaves {
        (action WaitForAttackEvent) does {
          onEventRule {
            (received EventAttack) -> { (v, f) =>
              // Do I need to process data in here if I just want to do a state transition based on the AppID?
            }
          }
        }
      } switch2 (state Evaluate_All_Banking_App) when ((resource MalAppID).getValues.head.toInt == ((dispatch EventAttack).fields.head).asInstanceOf[Int])
      // Switch to the next state only when the MalAppID matches the ones from the Simulator's message. 
      (state Evaluate_All_Banking_App) onSwitch {
        // TODO: Evaluate all banking apps on the global resource and decide the attack plan. Do when NetGraphSim for SEAPHISH is done
      } switch2 (state Attack_Target_App)
      (state Attack_Target_App) onSwitch {
        // TODO Attack the target app accordingly by modifying the ListOfMaliciousApp global resource accordingly. Do when NetGraphSim for SEAPHISH is done. 
        (dispatch AttackDone) send (channel Attacker_to_Sim)
      } switch2 (state Attacker_Move)
    } autotrigger (state WaitForAttackEvent)

    (model AttackerModel)
      `is defined as` {

    } `is defined as` {
      (agent MalApp) ~> (channel Attacker_to_Sim) ~> (agent Simulator)
      (agent Simulator) ~> (channel Sim_to_Attacker) ~> (agent MalApp)
    }

    val st = ValidationState.empty

    val stNew = summon[DialsValidator[DialsEntity]].processIR(ModelEntity().head, st)

    val res = ValidationResult()

    val res2 = summon[DialsValidator[DialsEntity]].validate(ModelEntity().head, stNew, res)


    ModelEntity.resetAll()
    }
  
  
