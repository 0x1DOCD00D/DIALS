/*
 * Copyright (newConnection) 7/6/24, 1:46 PM, 6. Mark Grechanik and Lone Star Consulting, Inc. All rights reserved.
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
class AgentEntityTest extends AnyFlatSpec with Matchers {
  val logger: Logger = CreateLogger(classOf[AgentEntityTest])
  behavior of "agents entities"

  it should "produce an intermediate representation of the ent definitions" in {
    (agent process1) has {
      (state st1) behaves {
//        behavior is already a keyword in scalatest so we can use the keyword action, alternatively
        (action b1) does {
          {case _ => ()}
        };
      } switch2 (state st2);

      (state st2) behaves {
        (action b5) does {
          {case _ => ()}
        };
      } switch2 (state st1)
    }

    (agent process2) has {
      (state onlyOneState) behaves (action b1).get switch2 (state onlyOneState)
    }

    (agent process3) has {
      (state onlyOneState) behaves {
        (action b3) does {
         { case _ => ()}
        }
      }
    }
    logger.info(AgentEntity.toString)
    AgentEntity() shouldBe List("process3", "process2", "process1")
    GlobalProcessingState.resetAll()
  }

  it should "generate three ent definitions" in {
    (agent process1) has {
      (state st1) behaves {
        (action b1).get orElse (action b2).get
      } switch2 (state st2) when {
        (resource X).getValues.toList.length > 0 && (resource Y).getValues.toList.head.toInt == 2} timeout 3.second ;
      (state st2) behaves {
        (action b5) does {
          {case _ => println("b5 in s2")}
        }
      } switch2 (state st1)
    }

    (agent process2) has {}

    (agent process3) has {
      (state _SingleState) behaves {
        case _ => ()
      };
      (state WrongState) behaves {
        case _ => ()
      };
    }

    logger.info(AgentEntity.toString)
    AgentEntity().head shouldBe "process3"
    GlobalProcessingState.resetAll()
  }

  it should "generate one ent definition with multiple actions" in {
    (agent process1) has {
      (state st2) behaves {
        (action b5) triggeredBy  {
          (Keywords.message m1)
          (Keywords.message m2)
        } triggeredBy {
          (Keywords.message m3)
          (Keywords.message m4)
          (Keywords.message m5)
        } does {
          {case _ => println("b5 in s2")}
        }
      } switch2 (state st1)
    }

    logger.info(AgentEntity.toString)
    AgentEntity().length shouldBe 1
    GlobalProcessingState.resetAll()
  }

  it should "generate one ent definition with a periodic behavior" in {
    (agent process1) has {
      (state st2) behaves {
        (action b5) triggeredBy {
          (Keywords.message m1)
          (Keywords.message m2)
        } triggeredBy {
          (Keywords.message m3)
          (Keywords.message m4)
          (Keywords.message m5)
        } does {
          {case _ => println("b5 in s2")}
        }
      } periodic (1, 2, -1)
    }

    logger.info(AgentEntity.toString)
    AgentEntity().length shouldBe 1
    GlobalProcessingState.resetAll()
  }

  it should "generate one ent definition with a timer attached to some states" in {
    (agent process1) has {
      (state st2) behaves {
        (action b5) triggeredBy {
          (Keywords.message m3)
          (Keywords.message m4)
          (Keywords.message m5)
        } does {
          {
            case _ => println("b5 in s2")
          }        }
      } switch2 (state st1) when always timeout 1.second
    }

    logger.info(AgentEntity.toString)
    AgentEntity().length shouldBe 1
    GlobalProcessingState.resetAll()
  }


  it should "create agents that can be auto triggered to start a simulation" in {
    (agent process1) has {
      (state st1) behaves {
        (action b1).get orElse (action b2).get
      } switch2 (state st2);

      (state st2) behaves {
        (action b5) does {
          {case _ => println("b5 in s2")}
        }
      } switch2 (state st1)
    } autotrigger (state st1);

    (agent process2) has {}

    (agent process3) has:
      (state _SingleState) behaves {
        case _ => ()
      };
      (state WrongState) behaves {
        case _ => ()
      };

    logger.info(AgentEntity.toString)
    AgentEntity.autoTriggeredAgents().map((a: AgentEntity, s:StateEntity) => (a.name, s.name)) shouldBe List(("process1", "st1"))
    GlobalProcessingState.resetAll()
  }

  it should "create an agent with three nondeterministic transitioned states" in {
    (agent process1) has {
      (state st1) behaves {
        (action b1).get orElse (action b2).get
      } switch2 (state st2);

      (state st2) behaves {
        (action b5) triggeredBy {
          (dispatch controlMsg)
          (dispatch shutdownMsg)
          } does {
          {case _ => (dispatch infoMsg) send (channel X)}
        } orElse (action b3).get
      } switch2 (state st1) when {
        (resource Storage).getValues.toList.head.toInt == 2
      } orSwitch2 (state st3) when (1==1) orSwitch2 (state st4) when (1==1);

      (state st3) behaves {
        (action b5).get
      } switch2 (state st3) when always timeout 10.seconds
    } autotrigger (state st1);

    logger.info(AgentEntity.toString)
    AgentEntity.autoTriggeredAgents().map((a: AgentEntity, s: StateEntity) => (a.name, s.name)) shouldBe List(("process1", "st1"))
    GlobalProcessingState.resetAll()
  }

}
