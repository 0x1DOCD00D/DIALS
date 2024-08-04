/*
 * Copyright (c) 7/3/24, 9:20 AM, 3. Mark Grechanik and Lone Star Consulting, Inc. All rights reserved.
 *
 *  Unless required by applicable law or agreed to in writing, software distributed under the License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *
 *  See the License for the specific language governing permissions and limitations under the License.
 */

package Utilz

import com.github.dwickern.macros.NameOf.nameOf

import java.text.SimpleDateFormat
import java.util.Date

object Constants:
  final val OpenParen = '('
  final val CloseParen = ')'
  final val OpenBra = "["
  final val CloseKet = "]"
  final val CommaSeparator = ','
  final val DotSeparator = "\\."
  final val Dot = "."
  final val ArgumentUnderscore = "_"
  final val EmptyBehaviorID = "__EmptyBehavior"
  final val AllChannelsID = "__AllChannels"
  final val SenderAgentID = "__SenderAgentID"
  val MathApacheDistributionsPkgName = "org.apache.commons.math3.distribution"
  val DefaultRandomGenerator: String = "Well1024a"

  val OUTPUTDIRECTORY: String = "outputDirectory"
  def OUTPUTFILENAME: String =
    val currentDate = new Date(System.currentTimeMillis())
    val df = new SimpleDateFormat("dd-MM-yy-HH-mm-ss")
    "Dials_" + df.format(currentDate) + ".txt"

  final val BetaDistribution: String = nameOf(BetaDistribution)
  final val BinomialDistribution: String = nameOf(BinomialDistribution)
  final val CauchyDistribution: String = nameOf(CauchyDistribution)
  final val ChiSquaredDistribution: String = nameOf(ChiSquaredDistribution)
  final val ExponentialDistribution: String = nameOf(ExponentialDistribution)
  final val FDistribution: String = nameOf(FDistribution)
  final val GammaDistribution: String = nameOf(GammaDistribution)
  final val GeometricDistribution: String = nameOf(GeometricDistribution)
  final val GumbelDistribution: String = nameOf(GumbelDistribution)
  final val HypergeometricDistribution: String = nameOf(HypergeometricDistribution)
  final val LaplaceDistribution: String = nameOf(LaplaceDistribution)
  final val LevyDistribution: String = nameOf(LevyDistribution)
  final val LogNormalDistribution: String = nameOf(LogNormalDistribution)
  final val LogisticDistribution: String = nameOf(LogisticDistribution)
  final val NakagamiDistribution: String = nameOf(NakagamiDistribution)
  final val NormalDistribution: String = nameOf(NormalDistribution)
  final val ParetoDistribution: String = nameOf(ParetoDistribution)
  final val PascalDistribution: String = nameOf(PascalDistribution)
  final val PoissonDistribution: String = nameOf(PoissonDistribution)
  final val TDistribution: String = nameOf(TDistribution)
  final val TriangularDistribution: String = nameOf(TriangularDistribution)
  final val UniformRealDistribution: String = nameOf(UniformRealDistribution)
  final val UniformIntegerDistribution: String = nameOf(UniformIntegerDistribution)
  final val WeibullDistribution: String = nameOf(WeibullDistribution)
  final val ZipfDistribution: String = nameOf(ZipfDistribution)
  final val EnumeratedIntegerDistribution: String = nameOf(EnumeratedIntegerDistribution)
