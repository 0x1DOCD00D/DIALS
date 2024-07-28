/*
 * Copyright (newConnection) 7/15/24, 9:14 AM, 15. Mark Grechanik and Lone Star Consulting, Inc. All rights reserved.
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

class ModelEntity private (
                            val name: String,
                            val cardinalities: ListBuffer[Cardinality] = ListBuffer.empty,
                            val connections: ListBuffer[Connection] = ListBuffer.empty) extends DialsEntity:
  override def toString: String = s"ModelEntity: $name" +
    (if connections.isEmpty then " has no connections"
      else s" with connections: ${connections.mkString(", ")}") +
      (if cardinalities.isEmpty then " has no cardinalities"
      else s" with cardinalities: ${cardinalities.mkString(", ")}")

  def addConnection(c: Connection): Unit = connections.prependAll(List(c))
  def addCardinality(c: Cardinality): Unit = cardinalities.prependAll(List(c))

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

  def resetConnectionChain(): Unit = _currentChain = EmptyConnection

  def currentChain: Connection = _currentChain

  def currentChain_=(newConnection: Connection): Unit =
    if modelEntities.isEmpty then throw new IllegalStateException("ModelEntity is in an invalid state: there is no model entity to add the connection to.")
    else if currentChain == EmptyConnection && newConnection.isInstanceOf[PartialConnection] then
      _currentChain = newConnection
    else if currentChain.isInstanceOf[PartialConnection] && newConnection.isInstanceOf[CompletedChain] then
      _currentChain = newConnection
      modelEntities.head.addConnection(newConnection)
    else if currentChain.isInstanceOf[PartialConnection] && newConnection.isInstanceOf[PartialConnection] then
      if ConfigDb.`DIALS.General.debugMode` then logger.error(s"ModelEntity is in an invalid state: $currentChain when switching to the new state $newConnection. Discarding this chain, resetting and continuing...")
      modelEntities.head.addConnection(currentChain)
      _currentChain = newConnection
    else if currentChain.isInstanceOf[CompletedChain] && (newConnection == EmptyConnection || newConnection.isInstanceOf[PartialConnection]) then
      _currentChain = newConnection
    else
      modelEntities.head.addConnection(_currentChain)
      _currentChain = EmptyConnection

  def apply(): List[ModelEntity] = modelEntities.toList

  def apply(entity: DialsEntity): Option[Cardinality] =
    if modelEntities.isEmpty then throw new IllegalStateException("ModelEntity is in an invalid state: there is no model entity to add the connection to.")
    else modelEntities.toList.head.cardinalities.find(_.entity == entity)
  
  
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

  def apply(c: Cardinality): Either[String, ModelEntity] =
    if modelEntities.isEmpty then Left("ModelEntity is in an invalid state: there is no model entity to add the cardinality to.")
    else
      modelEntities.head.addCardinality(c)
      Right(modelEntities.head)

  def resetAll(): Unit = 
    modelEntities.clear()
    resetConnectionChain()
    
  enum DIRECTION:
    case LEFT2RIGHT, RIGHT2LEFT, BIDIRECTIONAL

  def createPartialConnection(cc: CompleteConnection, c: ModelGraphEdge, d: DIRECTION): PartialConnection =
    currentChain = cc match
      case CompletedChain(left, right, channel, direction) =>
        direction match
          case BIDIRECTIONAL => BiDirectionalConnection(right, c)
          case LEFT2RIGHT => RightDirectional(right, c)
          case RIGHT2LEFT => LeftDirectional(left, c)
      case BadConnection(_) => CannotBuildPartialConnection
    currentChain.asInstanceOf[PartialConnection]

  def createPartialConnection(a: ModelGraphNodeIndexed, c: ModelGraphEdge, d: DIRECTION): PartialConnection =
    currentChain = d match
      case DIRECTION.BIDIRECTIONAL => BiDirectionalConnection(a, c)
      case DIRECTION.RIGHT2LEFT => LeftDirectional(a, c)
      case DIRECTION.LEFT2RIGHT => RightDirectional(a, c)

    currentChain.asInstanceOf[PartialConnection]

  def createCompleteConnection(pc: PartialConnection, b: ModelGraphNodeIndexed): CompleteConnection =
    currentChain = pc match
      case BiDirectionalConnection(left, channel) => CompletedChain(left, b, channel, BIDIRECTIONAL)
      case LeftDirectional(to, channel) => CompletedChain(b, to, channel, RIGHT2LEFT)
      case RightDirectional(from, channel) => CompletedChain(from, b, channel, LEFT2RIGHT)
      case puzzled => BadConnection(s"ModelEntity is in an invalid state: $puzzled. Discarding this chain, resetting and continuing...")
    val conn = currentChain.asInstanceOf[CompleteConnection]
    conn