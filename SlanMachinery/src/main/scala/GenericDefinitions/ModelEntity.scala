/*
 * Copyright (c) 7/15/24, 9:14 AM, 15. Mark Grechanik and Lone Star Consulting, Inc. All rights reserved.
 *
 *  Unless required by applicable law or agreed to in writing, software distributed under the License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *
 *  See the License for the specific language governing permissions and limitations under the License.
 */

package GenericDefinitions

import GenericDefinitions.ModelEntity.DIRECTION.{BIDIRECTIONAL, LEFT2RIGHT, RIGHT2LEFT}
import GenericDefinitions.ModelEntity.{createPartialConnection, logger}
import Utilz.ConfigDb
import scala.collection.mutable.ListBuffer

extension (aN: AgentEntity)
  infix def <~>(c: ModelGraphEdge): BiDirectionalConnection = createPartialConnection(aN, c, BIDIRECTIONAL).asInstanceOf[BiDirectionalConnection]
  infix def ~>(c: ModelGraphEdge): RightDirectional = createPartialConnection(aN, c, LEFT2RIGHT).asInstanceOf[RightDirectional]
  infix def <~(c: ModelGraphEdge): LeftDirectional = createPartialConnection(aN, c, RIGHT2LEFT).asInstanceOf[LeftDirectional]

class ModelEntity private (val name: String, val connections: ListBuffer[Connection] = ListBuffer.empty) extends DialsEntity:
  override def toString: String = s"ModelEntity: $name" +
    (if connections.isEmpty then " has no connections"
      else s" with connections: ${connections.mkString(", ")}")

  def addConnection(c: Connection): Unit = connections.prependAll(List(c))

  infix def `is defined as`(defModel: => Unit): ModelEntity =
    GlobalProcessingState(this) match
      case Left(errorMsg) =>
        logger.error(errorMsg)
      case Right(obj) =>
        defModel
        GlobalProcessingState(NoEntity) match
          case Left(errMsg) => logger.error(errMsg)
          case Right(_) =>
            if ConfigDb.`DIALS.General.debugMode` then logger.info(s"Setting the global processing state to $obj")
    this


object ModelEntity:
  private val modelEntities: ListBuffer[ModelEntity] = ListBuffer.empty

  private var _currentChain: Connection = EmptyConnection

  val logger = Utilz.CreateLogger(classOf[ModelEntity])

  override def toString: String = modelEntities.map(_.toString).mkString("\n")

  def currentChain: Connection = _currentChain
  def currentChain_=(c: Connection): Unit = 
    if modelEntities.isEmpty then throw new IllegalStateException("ModelEntity is in an invalid state: modelEntities is empty")
    else if c.isInstanceOf[CompletedChain] then 
      modelEntities.head.addConnection(_currentChain)  
      _currentChain = c
    else if c.isInstanceOf[PartialConnection] then
      _currentChain = c
    else
      modelEntities.head.addConnection(_currentChain)  
      _currentChain = EmptyConnection
    
  def apply(): List[String] = modelEntities.map(_.name).toList
  
  def apply(name: String): ModelEntity =
    val found = modelEntities.toList.find(_.name == name)
    if found.isEmpty then
      val nm = new ModelEntity(name)
      modelEntities.prependAll(List(nm))
      if ConfigDb.`DIALS.General.debugMode` then logger.info(s"Creating a model entity named $name")
      nm
    else
      if ConfigDb.`DIALS.General.debugMode` then logger.warn(s"ModelEntity with name $name already exists")
      found.get

  def resetAll(): Unit = modelEntities.clear()

  enum DIRECTION:
    case LEFT2RIGHT, RIGHT2LEFT, BIDIRECTIONAL
    
  def createPartialConnection(cc: CompleteConnection, c: ModelGraphEdge, d: DIRECTION): PartialConnection =
    if ModelEntity.currentChain != EmptyConnection then
      logger.error(s"ModelEntity is in an invalid state: ${ModelEntity.currentChain.toString}. Discarding this chain, resetting and continuing...")
      currentChain = EmptyConnection

    currentChain = cc match
      case CompletedChain(left, right, channel, direction) => 
        direction match
          case BIDIRECTIONAL => BiDirectionalConnection(right, c)
          case LEFT2RIGHT => RightDirectional(right, c)
          case RIGHT2LEFT => LeftDirectional(left, c)
      case BadConnection(_) => CannotBuildPartialConnection
    currentChain.asInstanceOf[PartialConnection]
      
  def createPartialConnection(a: ModelGraphNode, c: ModelGraphEdge, d: DIRECTION): PartialConnection =
    if ModelEntity.currentChain != EmptyConnection then
      logger.error(s"ModelEntity is in an invalid state: ${ModelEntity.currentChain.toString}. Discarding this chain, resetting and continuing...")
      currentChain = EmptyConnection

    currentChain =  d match
      case DIRECTION.BIDIRECTIONAL => BiDirectionalConnection(a, c)
      case DIRECTION.RIGHT2LEFT => LeftDirectional(a, c)
      case DIRECTION.LEFT2RIGHT => RightDirectional(a, c)
      
    currentChain.asInstanceOf[PartialConnection]

  def createCompleteConnection(pc: PartialConnection, b: ModelGraphNode): CompleteConnection =
    if !ModelEntity.currentChain.isInstanceOf[PartialConnection] then
      logger.error(s"ModelEntity is in an invalid state: ${ModelEntity.currentChain.toString}. Discarding this chain, resetting and continuing...")
      currentChain = EmptyConnection

    currentChain = pc match
      case BiDirectionalConnection(left, channel) => CompletedChain(left, b, channel, BIDIRECTIONAL) 
      case LeftDirectional(to, channel) => CompletedChain(b, to, channel, RIGHT2LEFT)
      case RightDirectional(from, channel) => CompletedChain(from, b, channel, LEFT2RIGHT)
      case puzzled => BadConnection(s"ModelEntity is in an invalid state: $puzzled. Discarding this chain, resetting and continuing...")
    val conn = currentChain.asInstanceOf[CompleteConnection]
    currentChain = EmptyConnection
    conn