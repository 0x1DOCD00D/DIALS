/*
 * Copyright (c) 6/2/24, 2:00 PM, 2. Mark Grechanik and Lone Star Consulting, Inc. All rights reserved.
 *
 *  Unless required by applicable law or agreed to in writing, software distributed under the License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *
 *  See the License for the specific language governing permissions and limitations under the License.
 */

package Pdfs

import Utilz.{ConfigDb, Constants, CreateLogger}
import com.google.common.reflect.ClassPath

import scala.util.{Failure, Success, Try}
import org.apache.commons.math3.random.*

import java.util.stream.Collectors
import scala.jdk.CollectionConverters.*

object RandomGeneratorMgr:
  val logger = CreateLogger(RandomGeneratorMgr.getClass)
  enum RandomGenerators:
    case ISAACRandom, JDKRandomGenerator, MersenneTwister, Well512a, Well1024a, Well19937a, Well19937c, Well44497a, Well44497b

  private def getRandomGenerator(generatorName: String): RandomGenerator =
    generatorName match
      case gn if gn.equalsIgnoreCase(RandomGenerators.ISAACRandom.toString.trim) => new ISAACRandom()
      case gn if gn.equalsIgnoreCase(RandomGenerators.JDKRandomGenerator.toString.trim) => new JDKRandomGenerator()
      case gn if gn.equalsIgnoreCase(RandomGenerators.MersenneTwister.toString.trim) => new MersenneTwister()
      case gn if gn.equalsIgnoreCase(RandomGenerators.Well512a.toString.trim) => new Well512a()
      case gn if gn.equalsIgnoreCase(RandomGenerators.Well1024a.toString.trim) => new Well1024a()
      case gn if gn.equalsIgnoreCase(RandomGenerators.Well19937a.toString.trim) => new Well19937a()
      case gn if gn.equalsIgnoreCase(RandomGenerators.Well19937c.toString.trim) => new Well19937c()
      case gn if gn.equalsIgnoreCase(RandomGenerators.Well44497a.toString.trim) => new Well44497a()
      case gn if gn.equalsIgnoreCase(RandomGenerators.Well44497b.toString.trim) => new Well44497b()
      case _ => throw new IllegalArgumentException(s"Random generator $generatorName is not supported.")

  def apply(): Either[String, RandomGenerator] =
    Try {
      getRandomGenerator(ConfigDb.`DIALS.Random.generator`)
    } match
      case Failure(exception) => Left(exception.getMessage)
      case Success(g) => g.setSeed(ConfigDb.`DIALS.Random.seed`)
          Right(g)
