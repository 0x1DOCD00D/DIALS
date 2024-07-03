/*
 * Copyright (c) 7/3/24, 9:14 AM, 3. Mark Grechanik and Lone Star Consulting, Inc. All rights reserved.
 *  
 *  Unless required by applicable law or agreed to in writing, software distributed under the License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  
 *  See the License for the specific language governing permissions and limitations under the License.
 */

package Utilz

import com.typesafe.config.ConfigFactory

import scala.util.{Failure, Try}

object ConfigReader:
  private val logger = CreateLogger(classOf[ConfigReader.type])

  case class EnumeratedLoopParameters(ps: List[Double])

  case class FromToWithStepParameters(from: Double, to: Double, step: Double)


  def getConfigEntry[T](entry: String, defValue: T): T =
    val cfg = ConfigFactory.load()
    val cv = defValue match
      case v: Int => Try(cfg.getInt(entry))
      case v: Long => Try(cfg.getLong(entry))
      case v: Boolean => Try(cfg.getBoolean(entry))
      case v: Double => Try(cfg.getDouble(entry))
      case EnumeratedLoopParameters(ps) => Try(cfg.getDoubleList(entry))
      case FromToWithStepParameters(from, to, step) => Try(cfg.getDoubleList(entry))
      case _ => Try(cfg.getString(entry))

    cv match {
      case scala.util.Success(value) =>
        Try(value) match {
          case scala.util.Success(value) =>
            logger.info(s"Loaded config entry $entry = $value")
            defValue match
              case _: EnumeratedLoopParameters => EnumeratedLoopParameters(value.asInstanceOf[List[Double]]).asInstanceOf[T]
              case _: FromToWithStepParameters =>
                if value.asInstanceOf[List[Double]].length == 3
                then FromToWithStepParameters(value.asInstanceOf[List[Double]].head,
                  value.asInstanceOf[List[Double]](1),
                  value.asInstanceOf[List[Double]](2)).asInstanceOf[T]
                else
                  defValue
              case _ => value.asInstanceOf[T]
          case scala.util.Failure(_) =>
            logger.info(s"Config entry $entry is absent, default value $defValue is used")
            defValue
        }
      case scala.util.Failure(_) =>
        logger.info(s"Config entry $entry is absent, default value $defValue is used")
        defValue
    }
