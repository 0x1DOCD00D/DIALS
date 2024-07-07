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

class AgentEntityTest extends AnyFlatSpec with Matchers {
  val logger: Logger = CreateLogger(classOf[AgentEntityTest])
  behavior of "dials entities"

  it should "do something here" in {
    (agent process1) has {
      
    }
    (agent process2) has {

    }
    AgentEntity() shouldBe List("process2", "process1")
  }
}
