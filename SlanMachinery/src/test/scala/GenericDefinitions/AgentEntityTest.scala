/*
 * Copyright (c) 7/6/24, 1:46 PM, 6. Mark Grechanik and Lone Star Consulting, Inc. All rights reserved.
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

@DoNotDiscover
class AgentEntityTest extends AnyFlatSpec with Matchers {
  val logger: Logger = CreateLogger(classOf[AgentEntityTest])
  behavior of "agents entities"

  it should "produce an intermediate representation of the agent definitions" in {
    (agent process1) has {
      (state st1) behaves {
//        behavior is already a keyword in scalatest so we can use the keyword action, alternatively
        (action b1) does {
          println("b1 in s1")
        };
        (action b2)
      } switch2 (state st2);
      (state st2) behaves {
        (action b5) does {}
      } switch2 (state st1)
    }

    (agent process2) has {
      (state onlyOneState) behaves (Keywords.behavior b1) switch2 (state onlyOneState)
    }

    (agent process3) has {
      (state onlyOneState) behaves {
        (action b3) does {
          ()
        } does {
          println("b3 in process3")
        }
      }
    }
    logger.info(AgentEntity.toString)
    AgentEntity() shouldBe List("process3", "process2", "process1")
  }

  it should "generate three agent definitions" in {
    (agent process1) has {
      (state st1) behaves {
        (action b1);
        (action b2)
      } switch2 (state st2);

      (state st2) behaves {
        (action b5) does {} does {}
      } switch2 (state st1)
    }

    (agent process2) has {}

    (agent process3) has {
      SingleState behaves {
        EmptyBehavior
      };
      (state WrongState) behaves {
        EmptyBehavior
      };
    }

    logger.info(AgentEntity.toString)
    AgentEntity().head shouldBe "process3"
  }
}
