package SimComponent

import org.scalatest.funsuite.AnyFunSuiteLike
import Utilz.{Constants, CreateLogger}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.slf4j.Logger

import scala.Dynamic
import scala.language.dynamics
import scala.language.postfixOps
import GenericDefinitions.*


import Validation.Results.ValidationResult
import Validation.DialsValidator
import Validation.States.ValidationState


import Keywords.*


class SimulatorTest extends AnyFlatSpec with Matchers{
  val logger_agent: Logger = CreateLogger(classOf[AgentEntityTest])
  val logger_group: Logger = CreateLogger(classOf[GroupEntityTest])
  it should "run" in {
//    Simulation.buildModel(5,5)
//    logger_agent.info(AgentEntity.toString)
//    logger_agent.info(GroupEntity.toString)
    Simulator.buildSimulator()
    ModelEntity.resetAll()
  }
}
