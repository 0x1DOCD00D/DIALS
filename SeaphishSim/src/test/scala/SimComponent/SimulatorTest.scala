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

import Keywords.*


class SimulatorTest extends AnyFlatSpec with Matchers{
  val logger: Logger = CreateLogger(classOf[GroupEntityTest])
  it should "run" in {
    Simulation.buildModel(3,3)
    logger.info(GroupEntity.toString)
  }
}
