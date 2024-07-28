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

extension (aN: ModelGraphNode)
  infix def <~>(c: ModelGraphEdge): BiDirectionalConnection = createPartialConnection((aN, 1), c, BIDIRECTIONAL).asInstanceOf[BiDirectionalConnection]
  infix def ~>(c: ModelGraphEdge): RightDirectional = createPartialConnection((aN, 1), c, LEFT2RIGHT).asInstanceOf[RightDirectional]
  infix def <~(c: ModelGraphEdge): LeftDirectional = createPartialConnection((aN, 1), c, RIGHT2LEFT).asInstanceOf[LeftDirectional]

extension (aN: ModelGraphNodeIndexed)
  infix def <~>(c: ModelGraphEdge): BiDirectionalConnection = createPartialConnection(aN, c, BIDIRECTIONAL).asInstanceOf[BiDirectionalConnection]
  infix def ~>(c: ModelGraphEdge): RightDirectional = createPartialConnection(aN, c, LEFT2RIGHT).asInstanceOf[RightDirectional]
  infix def <~(c: ModelGraphEdge): LeftDirectional = createPartialConnection(aN, c, RIGHT2LEFT).asInstanceOf[LeftDirectional]

case class BiDirectionalConnection(left: ModelGraphNodeIndexed, channel: ModelGraphEdge) extends PartialConnection:
  infix def <~>(right: ModelGraphNodeIndexed): CompletedChain =
    val cc = CompletedChain(left, right, channel, BIDIRECTIONAL)
    ModelEntity.currentChain = cc
    cc

  infix def <~>(right: ModelGraphNode): CompletedChain =
    val cc = CompletedChain(left, (right,1), channel, BIDIRECTIONAL)
    ModelEntity.currentChain = cc
    cc

case class RightDirectional(from: ModelGraphNodeIndexed, channel: ModelGraphEdge) extends PartialConnection:
  infix def ~>(to: ModelGraphNodeIndexed): CompletedChain = 
    val cc = CompletedChain(from, to, channel, LEFT2RIGHT)
    ModelEntity.currentChain = cc
    cc

  infix def ~>(to: ModelGraphNode): CompletedChain =
    val cc = CompletedChain(from, (to,1), channel, LEFT2RIGHT)
    ModelEntity.currentChain = cc
    cc

case class LeftDirectional(to: ModelGraphNodeIndexed, channel: ModelGraphEdge) extends PartialConnection:
  infix def <~(from: ModelGraphNodeIndexed): CompletedChain = 
    val cc = CompletedChain(from, to, channel, RIGHT2LEFT)
    ModelEntity.currentChain = cc
    cc

  infix def <~(from: ModelGraphNode): CompletedChain =
    val cc = CompletedChain((from,1), to, channel, RIGHT2LEFT)
    ModelEntity.currentChain = cc
    cc

case class CompletedChain(from: ModelGraphNodeIndexed, to: ModelGraphNodeIndexed, channel: ModelGraphEdge, d: ModelEntity.DIRECTION) extends CompleteConnection:
  infix def <~>(c: ModelGraphEdge): BiDirectionalConnection = createPartialConnection(to, c, BIDIRECTIONAL).asInstanceOf[BiDirectionalConnection]
  infix def ~>(c: ModelGraphEdge): RightDirectional = createPartialConnection(to, c, LEFT2RIGHT).asInstanceOf[RightDirectional]
  infix def <~(c: ModelGraphEdge): LeftDirectional = createPartialConnection(to, c, RIGHT2LEFT).asInstanceOf[LeftDirectional]

