/*
 * Copyright (c) 8/6/24, 12:29 PM, 6. Mark Grechanik and Lone Star Consulting, Inc. All rights reserved.
 *
 *  Unless required by applicable law or agreed to in writing, software distributed under the License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *
 *  See the License for the specific language governing permissions and limitations under the License.
 */

package GenericDefinitions

import Pdfs.PdfStreamGenerator
import Utilz.CreateLogger

import scala.reflect.ClassTag
import scala.compiletime.{erasedValue, summonFrom}

object InputDataProcessor:
  val logger = CreateLogger(classOf[InputDataProcessor.type])
  trait TypeInfo[T]:
    def typeStuff: Class[?]

  given [T](using ct: ClassTag[T]): TypeInfo[T] with {
    def typeStuff: Class[?] = ct.runtimeClass
  }

  def apply[E <: AnyVal | DialsEntity | DistributionEntity](elems: E*)(using ti: TypeInfo[E]): (LazyList[Double], List[DialsEntity]) =
    ti.typeStuff match
      case c if c == classOf[Int] => (elems.map(_.asInstanceOf[Int].toDouble).to(LazyList), List.empty)
      case c if c == classOf[Float] => (elems.map(_.asInstanceOf[Float].toDouble).to(LazyList), List.empty)
      case c if c == classOf[Double] => (elems.map(_.asInstanceOf[Double]).to(LazyList), List.empty)
      case c if c == classOf[DialsEntity] => (LazyList.empty, elems.map(_.asInstanceOf[DialsEntity]).toList)
      case c if c == classOf[DistributionEntity] =>
        val name = elems(0).asInstanceOf[DistributionEntity].name
        val parmz = elems(0).asInstanceOf[DistributionEntity].params.map {
          case i: Int => i.toDouble
          case f: Float => f.toDouble
          case d: Double => d
          case other =>
            logger.error(s"Assigning data of the type $other to a list of numbers is not supported.")
            throw new NumberFormatException(s"Assigning data of the type $other to a list of numbers is not supported.")
        }
        (PdfStreamGenerator(name, parmz), List.empty)
      case error =>
        logger.error(s"Assigning data of the type $error to a list of numbers is not supported.")
        (LazyList.empty, List.empty)
