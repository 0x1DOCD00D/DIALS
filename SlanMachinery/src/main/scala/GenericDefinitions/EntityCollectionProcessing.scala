/*
 * Copyright (c) 7/26/24, 8:07 PM, 26. Mark Grechanik and Lone Star Consulting, Inc. All rights reserved.
 *
 *  Unless required by applicable law or agreed to in writing, software distributed under the License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *
 *  See the License for the specific language governing permissions and limitations under the License.
 */

package GenericDefinitions

import GenericDefinitions.EntityCollectionProcessing.logger
import Utilz.CreateLogger

extension [T <: DialsEntity](entity: T)
  def index(whichOne: Int => Int = identity): T = ???
  def first: T = ???
  def last: T = ???


def condition(cond: => Boolean): A = new A(cond)

class A(cond: => Boolean):
  infix def ->(b: => Unit): Unit = if cond then b else ()

class EntityCollectionProcessing

object EntityCollectionProcessing:
  @main def runEntityCollectionProcessing(args: String*): Unit =
    println("File /Users/drmark/Library/CloudStorage/OneDrive-UniversityofIllinoisChicago/Github/DIALS/SlanMachinery/src/main/scala/GenericDefinitions/EntityCollectionProcessing.scala created at time 8:22 PM")
    condition(1 == 2) -> logger.info("The condition is true")

  val logger = CreateLogger(classOf[EntityCollectionProcessing])
  def apply(): EntityCollectionProcessing = new EntityCollectionProcessing()