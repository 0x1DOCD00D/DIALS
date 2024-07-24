/*
 * Copyright (c) 7/22/24, 7:47 PM, 22. Mark Grechanik and Lone Star Consulting, Inc. All rights reserved.
 *  
 *  Unless required by applicable law or agreed to in writing, software distributed under the License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  
 *  See the License for the specific language governing permissions and limitations under the License.
 */

package GenericDefinitions

import GenericDefinitions.CardinalityOfDialsEntity.logger
import Utilz.CreateLogger

import scala.Dynamic
import scala.collection.mutable.ListBuffer
import scala.compiletime.uninitialized
import scala.language.dynamics
import scala.language.postfixOps

sealed trait ConditionSpecifier
case object none extends ConditionSpecifier
case object := extends ConditionSpecifier
case object less extends ConditionSpecifier
case object greater extends ConditionSpecifier

case class Cardinality(entity: DialsEntity, value: EntityValue)

sealed trait EntityValue
case class EntityPdfValue(value: DialsEntity) extends EntityValue
case class EntityIntValue(value: Int) extends EntityValue
case class EntityRangeOfValues(from: Int, to: Int) extends EntityValue

class BetweenFromTo(val entity: DialsEntity, val from: Int):
  infix def and(to: Int): Unit =
    require(to > from, "The upper bound must be greater than the lower bound")
    ModelEntity(Cardinality(entity, EntityRangeOfValues(from, to)))
    ()

class Approximately(val entity: DialsEntity, val base: Int):
  infix def `plus or minus`(precisionPercent: Int): Unit =
    require(precisionPercent >= 0 && precisionPercent <= 100, "The precision percentage must be between 0 and 100")
    val precision = base.asInstanceOf[Double] * precisionPercent / 100
    val from = if base - precision >= 0 then base - precision else 0
    val to = base + precision
    ModelEntity(Cardinality(entity, EntityRangeOfValues(from.toInt, to.toInt)))
    ()

def | [T <: DialsEntity](entity: T): CardinalityOfDialsEntity = CardinalityOfDialsEntity(entity)

class NoCardinality:
  infix def exactly[T](value: T): Unit =
    logger.error(s"The cardinality instruction is not defined for value $value")

class EqualToSomething(val entity: DialsEntity):
  infix def exactly[T <: DialsEntity | Int](value: T): Unit =
    value match
      case vpdf: DialsEntity => ModelEntity(Cardinality(entity, EntityPdfValue(vpdf)))
      case vint: Int => ModelEntity(Cardinality(entity, EntityIntValue(vint)))
    ()
  infix def approximately(value: Int): Approximately = new Approximately(entity, value)
  infix def between(from: Int): BetweenFromTo = new BetweenFromTo(entity, from)

class LessThanOrEqualToSomething(val entity: DialsEntity):
  infix def than[T](value: T): Unit  =
    ModelEntity(Cardinality(entity, EntityRangeOfValues(0, value.asInstanceOf[Int])))
    ()
  
class GreaterThanOrEqualToSomething(val entity: DialsEntity):
  infix def than[T](value: T): Unit  =
    ModelEntity(Cardinality(entity, EntityRangeOfValues(value.asInstanceOf[Int], Int.MaxValue)))
    ()

class CardinalityOfDialsEntity private (var entity: DialsEntity):
  infix def |(p: :=.type): EqualToSomething = new EqualToSomething(entity)
  infix def |(p: none.type): NoCardinality = NoCardinality()
  infix def |(p: less.type): LessThanOrEqualToSomething = new LessThanOrEqualToSomething(entity)
  infix def |(p: greater.type): GreaterThanOrEqualToSomething = new GreaterThanOrEqualToSomething(entity)
    
  infix def exactly[T](value: T): Unit  = ()
  infix def than[T](value: T): Unit  = ()
  

object CardinalityOfDialsEntity:
  val logger = CreateLogger(classOf[CardinalityOfDialsEntity])
  def apply(entity: DialsEntity): CardinalityOfDialsEntity = new CardinalityOfDialsEntity(entity)
