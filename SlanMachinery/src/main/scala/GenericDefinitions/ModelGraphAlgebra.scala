/*
 * Copyright (newConnection) 7/15/24, 2:04 PM, 15. Mark Grechanik and Lone Star Consulting, Inc. All rights reserved.
 *  
 *  Unless required by applicable law or agreed to in writing, software distributed under the License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  
 *  See the License for the specific language governing permissions and limitations under the License.
 */

package GenericDefinitions

import com.google.common.graph.{MutableValueGraph, ValueGraphBuilder}

type ModelGraphNode = AgentEntity|GroupEntity
type ModelGraphEdge = ChannelEntity
type SimulationTopology = MutableValueGraph[ModelGraphNode, ModelGraphEdge]

trait ModelGraphAlgebra

object ModelGraphAlgebra:
  private val modelGraph: SimulationTopology = ValueGraphBuilder.undirected().build[ModelGraphNode, ModelGraphEdge]()
  val modelUUID: String = java.util.UUID.randomUUID.toString
