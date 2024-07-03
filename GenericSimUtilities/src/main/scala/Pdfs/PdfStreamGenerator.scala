/*
 * Copyright (c) 6/2/24, 1:59 PM, 2. Mark Grechanik and Lone Star Consulting, Inc. All rights reserved.
 *
 *  Unless required by applicable law or agreed to in writing, software distributed under the License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *
 *  See the License for the specific language governing permissions and limitations under the License.
 */

package Pdfs

import Utilz.{ConfigDb, Constants}
import com.google.common.reflect.ClassPath
import org.apache.commons.math3.distribution.*
import org.apache.commons.math3.linear.{MatrixUtils, RealMatrix}
import org.apache.commons.math3.random.*
import org.slf4j.Logger

import java.util.stream.Collectors
import scala.jdk.CollectionConverters.*
import scala.util.hashing.MurmurHash3
import scala.util.{Failure, Success, Try}

type DistributionType = AbstractIntegerDistribution | AbstractRealDistribution

object PdfStreamGenerator:
  def apply(distName: String, params: Array[Double]): DistributionType =
    val rg = RandomGeneratorMgr()
    val distClass = ClassPath
      .from(ClassLoader.getSystemClassLoader)
      .getAllClasses
      .asScala
      .toList
      .filter(c => c.getPackageName.equalsIgnoreCase(Constants.MathApacheDistributionsPkgName)).find(c => c.getSimpleName.equalsIgnoreCase(distName)) match
        case Some(clzzRes) => Try(clzzRes.load()) match
          case Failure(exception) => throw new IllegalArgumentException(s"Class $distName cannot be loaded from package ${Constants.MathApacheDistributionsPkgName}")
          case Success(clzzDist) => clzzDist
        case None => throw new IllegalArgumentException(s"Class $distName is found in package ${Constants.MathApacheDistributionsPkgName}")

    distName match {
      case Constants.BetaDistribution
           | Constants.CauchyDistribution
           | Constants.FDistribution
           | Constants.GammaDistribution
           | Constants.GumbelDistribution
           | Constants.LaplaceDistribution
           | Constants.LevyDistribution
           | Constants.LogNormalDistribution
           | Constants.LogisticDistribution
           | Constants.NormalDistribution
           | Constants.ParetoDistribution
           | Constants.UniformRealDistribution
           | Constants.WeibullDistribution => distClass
              .getDeclaredConstructor(classOf[RandomGenerator], classOf[Double], classOf[Double])
              .newInstance(rg, params(0), params(1))
              .asInstanceOf[AbstractRealDistribution]
      case Constants.BinomialDistribution
           | Constants.ZipfDistribution
           | Constants.PascalDistribution => distClass
              .getDeclaredConstructor(classOf[RandomGenerator], classOf[Int], classOf[Double])
              .newInstance(rg, params(0).toInt, params(1))
              .asInstanceOf[AbstractRealDistribution]
      case Constants.ChiSquaredDistribution
           | Constants.ExponentialDistribution
           | Constants.GeometricDistribution
           | Constants.TDistribution => distClass
                .getDeclaredConstructor(classOf[RandomGenerator], classOf[Double])
                .newInstance(rg, params(0))
                .asInstanceOf[AbstractIntegerDistribution]
      case Constants.HypergeometricDistribution => distClass
        .getDeclaredConstructor(classOf[RandomGenerator], classOf[Int], classOf[Int], classOf[Int])
        .newInstance(rg, params(0).toInt, params(1).toInt, params(2).toInt)
        .asInstanceOf[AbstractIntegerDistribution]

      case Constants.NakagamiDistribution => distClass
        .getDeclaredConstructor(classOf[RandomGenerator], classOf[Double], classOf[Double], classOf[Double])
        .newInstance(rg, params(0), params(1), NakagamiDistribution.DEFAULT_INVERSE_ABSOLUTE_ACCURACY)
        .asInstanceOf[AbstractRealDistribution]

      case Constants.PoissonDistribution => distClass
        .getDeclaredConstructor(classOf[RandomGenerator], classOf[Double], classOf[Double], classOf[Int])
        .newInstance(rg, params(0), PoissonDistribution.DEFAULT_EPSILON, PoissonDistribution.DEFAULT_MAX_ITERATIONS)
        .asInstanceOf[AbstractIntegerDistribution]

      case Constants.TriangularDistribution => distClass
        .getDeclaredConstructor(classOf[RandomGenerator], classOf[Double], classOf[Double], classOf[Double])
        .newInstance(rg, params(0), params(1), params(2))
        .asInstanceOf[AbstractRealDistribution]

      case Constants.UniformIntegerDistribution => distClass
        .getDeclaredConstructor(classOf[RandomGenerator], classOf[Int], classOf[Int])
        .newInstance(rg, params(0).toInt, params(1).toInt)
        .asInstanceOf[AbstractIntegerDistribution]
    }
