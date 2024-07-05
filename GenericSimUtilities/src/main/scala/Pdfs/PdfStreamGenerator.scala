/*
 * Copyright (c) 6/2/24, 1:59 PM, 2. Mark Grechanik and Lone Star Consulting, Inc. All rights reserved.
 *
 *  Unless required by applicable law or agreed to in writing, software distributed under the License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *
 *  See the License for the specific language governing permissions and limitations under the License.
 */

package Pdfs

import Utilz.{ConfigDb, Constants, CreateLogger}
import com.google.common.reflect.ClassPath
import org.apache.commons.math3.distribution.*
import org.apache.commons.math3.linear.{MatrixUtils, RealMatrix}
import org.apache.commons.math3.random.*
import org.slf4j.Logger

import java.util.stream.Collectors
import scala.jdk.CollectionConverters.*
import scala.util.hashing.MurmurHash3
import scala.util.{Failure, Success, Try}

type DistributionType = Either[String, AbstractIntegerDistribution | AbstractRealDistribution]
class PdfStreamGeneratorException(message: String) extends Exception(message)

object PdfStreamGenerator:
  val logger: Logger = CreateLogger(classOf[PdfStreamGenerator.type])
  val randGen: Either[String, RandomGenerator] = RandomGeneratorMgr()

  private def generateSamples(generator: DistributionType): LazyList[Double] =
    generator match
      case Left(errorMessage) =>
        if errorMessage != null then throw new PdfStreamGeneratorException(errorMessage) else throw new PdfStreamGeneratorException("RandomGenerator failed.")
        
      case Right(obj) => obj match {
          case v: AbstractIntegerDistribution => v.sample() #:: generateSamples(generator)
          case v: AbstractRealDistribution => v.sample() #:: generateSamples(generator)
        }
  end generateSamples


  def apply(distName: String, params: Array[Double], intSequence: Array[Int] = Array()): LazyList[Double] =
    if randGen.isLeft then throw new PdfStreamGeneratorException(randGen.left.getOrElse("RandomGenerator is not available"))

    val rg = randGen match
      case Right(rng) => rng
      case Left(_) => throw new PdfStreamGeneratorException("This should not happen.")

    val distClass = Class.forName(s"${Constants.MathApacheDistributionsPkgName}.$distName")

    val dist = distName match {
      case eid if eid == Constants.EnumeratedIntegerDistribution =>
        if params.length != intSequence.length then Left("EnumeratedIntegerDistribution requires two arrays of the same length")
        else Try(new EnumeratedIntegerDistribution(rg, intSequence, params)) match
          case Success(dist) => Right(dist)
          case Failure(exception) => Left(exception.getMessage)

      case p2 if p2.compareToIgnoreCase(Constants.BetaDistribution) == 0
           | p2.compareToIgnoreCase(Constants.CauchyDistribution) == 0
           | p2.compareToIgnoreCase(Constants.FDistribution) == 0
           | p2.compareToIgnoreCase(Constants.GammaDistribution) == 0
           | p2.compareToIgnoreCase(Constants.GumbelDistribution) == 0
           | p2.compareToIgnoreCase(Constants.LaplaceDistribution) == 0
           | p2.compareToIgnoreCase(Constants.LevyDistribution) == 0
           | p2.compareToIgnoreCase(Constants.LogNormalDistribution) == 0
           | p2.compareToIgnoreCase(Constants.LogisticDistribution) == 0
           | p2.compareToIgnoreCase(Constants.NormalDistribution) == 0
           | p2.compareToIgnoreCase(Constants.ParetoDistribution) == 0
           | p2.compareToIgnoreCase(Constants.UniformRealDistribution) == 0
           | p2.compareToIgnoreCase(Constants.WeibullDistribution) == 0 => Try {
        distClass
          .getDeclaredConstructor(classOf[RandomGenerator], classOf[Double], classOf[Double])
          .newInstance(rg, params(0), params(1))
          .asInstanceOf[AbstractRealDistribution]
      } match
        case Success(dist) => Right(dist)
        case Failure(exception) => Left(exception.getMessage)

      case p1int if p1int.compareToIgnoreCase(Constants.BinomialDistribution) == 0
           | p1int.compareToIgnoreCase(Constants.ZipfDistribution) == 0
           | p1int.compareToIgnoreCase(Constants.PascalDistribution) == 0 => Try {
        distClass
          .getDeclaredConstructor(classOf[RandomGenerator], classOf[Int], classOf[Double])
          .newInstance(rg, params(0).toInt, params(1))
          .asInstanceOf[AbstractIntegerDistribution]
      } match
        case Success(dist) => Right(dist)
        case Failure(exception) =>
          logger.error(exception.getMessage)
          Left(exception.getMessage)

      case p1 if p1.compareToIgnoreCase(Constants.ChiSquaredDistribution) == 0
           | p1.compareToIgnoreCase(Constants.ExponentialDistribution) == 0
           | p1.compareToIgnoreCase(Constants.GeometricDistribution) == 0
           | p1.compareToIgnoreCase(Constants.TDistribution) == 0 => Try {
        val rd = distClass
          .getDeclaredConstructor(classOf[RandomGenerator], classOf[Double])
          .newInstance(rg, params(0))
        if p1.compareToIgnoreCase(Constants.GeometricDistribution) == 0 then rd.asInstanceOf[AbstractIntegerDistribution]
        else rd.asInstanceOf[AbstractRealDistribution]
      } match
        case Success(dist) => Right(dist)
        case Failure(exception) => Left(exception.getMessage)

      case hd if hd.compareToIgnoreCase(Constants.HypergeometricDistribution) == 0 => Try {
        distClass
          .getDeclaredConstructor(classOf[RandomGenerator], classOf[Int], classOf[Int], classOf[Int])
          .newInstance(rg, params(0).toInt, params(1).toInt, params(2).toInt)
          .asInstanceOf[AbstractIntegerDistribution]
      } match
        case Success(dist) => Right(dist)
        case Failure(exception) => Left(exception.getMessage)

      case nd if nd.compareToIgnoreCase(Constants.NakagamiDistribution) == 0 => Try {
        distClass
          .getDeclaredConstructor(classOf[RandomGenerator], classOf[Double], classOf[Double], classOf[Double])
          .newInstance(rg, params(0), params(1), NakagamiDistribution.DEFAULT_INVERSE_ABSOLUTE_ACCURACY)
          .asInstanceOf[AbstractRealDistribution]
      } match
        case Success(dist) => Right(dist)
        case Failure(exception) => Left(exception.getMessage)

      case pd if pd.compareToIgnoreCase(Constants.PoissonDistribution) == 0 => Try {
        distClass
          .getDeclaredConstructor(classOf[RandomGenerator], classOf[Double], classOf[Double], classOf[Int])
          .newInstance(rg, params(0), PoissonDistribution.DEFAULT_EPSILON, PoissonDistribution.DEFAULT_MAX_ITERATIONS)
          .asInstanceOf[AbstractIntegerDistribution]
      } match
        case Success(dist) => Right(dist)
        case Failure(exception) => Left(exception.getMessage)

      case td if td.compareToIgnoreCase(Constants.TriangularDistribution) == 0 => Try {
        distClass
          .getDeclaredConstructor(classOf[RandomGenerator], classOf[Double], classOf[Double], classOf[Double])
          .newInstance(rg, params(0), params(1), params(2))
          .asInstanceOf[AbstractRealDistribution]
      } match
        case Success(dist) => Right(dist)
        case Failure(exception) => Left(exception.getMessage)

      case uid if uid.compareToIgnoreCase(Constants.UniformIntegerDistribution) == 0 => Try {
        distClass
          .getDeclaredConstructor(classOf[RandomGenerator], classOf[Int], classOf[Int])
          .newInstance(rg, params(0).toInt, params(1).toInt)
          .asInstanceOf[AbstractIntegerDistribution]
      } match
        case Success(dist) => Right(dist)
        case Failure(exception) => Left(exception.getMessage)
    }
    generateSamples(dist)