/*
 * Copyright (newConnection) 7/20/24, 1:59 PM, 20. Mark Grechanik and Lone Star Consulting, Inc. All rights reserved.
 *  
 *  Unless required by applicable law or agreed to in writing, software distributed under the License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  
 *  See the License for the specific language governing permissions and limitations under the License.
 */

package GenericDefinitions

import GenericDefinitions.ModelEntity.DIRECTION.{BIDIRECTIONAL, LEFT2RIGHT, RIGHT2LEFT}
import GenericDefinitions.ModelEntity.createPartialConnection

trait Connection

case object EmptyConnection extends Connection
trait PartialConnection extends Connection
case object CannotBuildPartialConnection extends PartialConnection
trait CompleteConnection extends Connection
case class BadConnection(msg: String) extends CompleteConnection

case class BiDirectionalConnection(left: ModelGraphNode, channel: ModelGraphEdge) extends PartialConnection:
  infix def <~>(right: ModelGraphNode): CompletedChain =
    val cc = CompletedChain(left, right, channel, BIDIRECTIONAL)
    ModelEntity.currentChain = cc
    cc

case class RightDirectional(from: ModelGraphNode, channel: ModelGraphEdge) extends PartialConnection:
  infix def ~>(to: ModelGraphNode): CompletedChain = 
    val cc = CompletedChain(from, to, channel, LEFT2RIGHT)
    ModelEntity.currentChain = cc
    cc

case class LeftDirectional(to: ModelGraphNode, channel: ModelGraphEdge) extends PartialConnection:
  infix def <~(from: ModelGraphNode): CompletedChain = 
    val cc = CompletedChain(from, to, channel, RIGHT2LEFT)
    ModelEntity.currentChain = cc
    cc


case class CompletedChain(from: ModelGraphNode, to: ModelGraphNode, channel: ModelGraphEdge, d: ModelEntity.DIRECTION) extends CompleteConnection:
  infix def <~>(c: ModelGraphEdge): BiDirectionalConnection = createPartialConnection(to, c, BIDIRECTIONAL).asInstanceOf[BiDirectionalConnection]
  infix def ~>(c: ModelGraphEdge): RightDirectional = createPartialConnection(to, c, LEFT2RIGHT).asInstanceOf[RightDirectional]
  infix def <~(c: ModelGraphEdge): LeftDirectional = createPartialConnection(to, c, RIGHT2LEFT).asInstanceOf[LeftDirectional]

