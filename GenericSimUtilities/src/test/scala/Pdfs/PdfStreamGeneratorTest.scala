package Pdfs

import Pdfs.PdfStreamGenerator
import Utilz.{Constants, CreateLogger}
import org.apache.commons.math3.distribution.{BetaDistribution, BinomialDistribution, CauchyDistribution, ChiSquaredDistribution, ExponentialDistribution, FDistribution, GammaDistribution, GeometricDistribution, GumbelDistribution, HypergeometricDistribution, ParetoDistribution, PascalDistribution, PoissonDistribution, TDistribution, TriangularDistribution, ZipfDistribution}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import scala.util.hashing.MurmurHash3

class PdfStreamGeneratorTest extends AnyFlatSpec with Matchers {
  val logger = CreateLogger(classOf[PdfStreamGeneratorTest])
  behavior of "random number generator"

  it should "create hash codes from arrays of doubles" in {
    MurmurHash3.orderedHash(Array(1.12, 2.87, 52, 187)) shouldBe MurmurHash3.orderedHash(Array(1.12, 2.87, 52, 187))
    MurmurHash3.orderedHash(Array(1.12, 2.87, 52, 187)) should not be MurmurHash3.orderedHash(Array(1.12, 2.871, 52, 187))
  }

  /*
  * The Beta distribution is the conjugate prior for the Bernoulli, binomial, negative binomial and geometric distributions
  * (seems like those are the distributions that involve success & failure) in Bayesian inference. Computing a posterior using
  * a conjugate prior is very convenient, because you can avoid expensive numerical computation involved in Bayesian Inference.
  * Beta distribution can be understood as representing a distribution of probabilities, that is, it represents all the possible
  * values of a probability when we don't know what that probability is.

  * The beta distribution is a family of continuous probability distributions defined on the interval [0, 1] parameterized by
  * two positive shape parameters, denoted by α and β, that appear as exponents of the random variable and control the shape
  * of the distribution. The generalization to multiple variables is called a Dirichlet distribution.
  * We can use it to model the probabilities: the Click-Through Rate of your advertisement, the conversion rate of customers
  * actually purchasing on your website, how likely readers will clap for your blog, the 5-year survival chance for women with breast cancer.
  * Interpretation of α, β: α-1 as the number of successes and β-1 as the number of failures, just like n & n-x terms in binomial.
  * We can choose the α and β parameters this way: the probability of success is very high, let’s say 90%, set 90 for α and 10 for β.
  * Otherwise, 90 for β and 10 for α. As α becomes larger (more successful events), the bulk of the probability distribution will shift
  * towards the right, whereas an increase in β moves the distribution towards the left (more failures). Also, the distribution
  * will narrow if both α and β increase, for we are more certain.
  * https://towardsdatascience.com/beta-distribution-intuition-examples-and-derivation-cf00f4db57af
  * */

  it should "create two Beta distributions and check for the relative validity of the generated probabilities" in {
    //90% success and 10% failure
    logger.info("Running a beta dist test...")
    val seq1 = PdfStreamGenerator(Constants.BetaDistribution, Array(90d, 10d)).take(100).toList
    val seq2 = PdfStreamGenerator(Constants.BetaDistribution, Array(10d, 90d)).take(100).toList
    seq1.sum/100 should be > seq2.sum/100
  }

  it should "create two binomial distribution and check for the relative validity of success values" in {
    val seq1 = PdfStreamGenerator(Constants.BinomialDistribution, Array(1000, 0.07)).take(100).toList
    val seq2 = PdfStreamGenerator(Constants.BinomialDistribution, Array(1000, 0.7)).take(100).toList
    seq1.sum / 100 should be < seq2.sum / 100
  }

  it should "create two chi-squared distributions with different dfs and compare their samples" in {
    val dist1 = PdfStreamGenerator(Constants.ChiSquaredDistribution, Array(3)).take(1).toList
    val dist2 = PdfStreamGenerator("ChiSquaredDistribution", Array(1000)).take(1).toList
    dist1.head should be < dist2.head
  }

  it should "create two cauchy distributions and check for the relative validity of the generated probabilities" in {
    val seq1 = PdfStreamGenerator(Constants.CauchyDistribution, Array(90d, 10d)).take(100).toList
    val seq2 = PdfStreamGenerator(Constants.CauchyDistribution, Array(10d, 90d)).take(100).toList
    seq1.sum / 100 should be > seq2.sum / 100
  }

  it should "create two exponential distributions and check for the relative validity of the generated probabilities" in {
    //The exponential distribution occurs naturally when describing the lengths of the inter-arrival times in a homogeneous Poisson process.
    val seq1 = PdfStreamGenerator(Constants.ExponentialDistribution, Array(10)).take(100).toList
    val seq2 = PdfStreamGenerator(Constants.ExponentialDistribution, Array(100)).take(100).toList
    seq1.sum / 100 should be < seq2.sum / 100
  }

  it should "create two F-distributions and check for the relative validity of the generated probabilities" in {
    val seq1 = PdfStreamGenerator(Constants.FDistribution, Array(1,1)).take(100).toList
    val seq2 = PdfStreamGenerator(Constants.FDistribution, Array(10,1)).take(100).toList
    seq1.sum / 100 should be < seq2.sum / 100
  }

  it should "create two geometric distributions and check for the relative validity of the generated probabilities" in {
    //The probability distribution of the number X of Bernoulli trials needed to get one success, supported on N = { 1 , 2 , 3 , … }
    val seq1 = PdfStreamGenerator(Constants.GeometricDistribution, Array(0.07)).take(100).toList
    val seq2 = PdfStreamGenerator(Constants.GeometricDistribution, Array(0.7)).take(100).toList
    seq1.sum / 100 should be > seq2.sum / 100
  }

  it should "create two Gamma distributions and check for the relative validity of the generated probabilities" in {
    val seq1 = PdfStreamGenerator(Constants.GammaDistribution, Array(1, 2)).take(100).toList
    val seq2 = PdfStreamGenerator(Constants.GammaDistribution, Array(10, 1)).take(100).toList
    seq1.sum / 100 should be < seq2.sum / 100
  }

  it should "create two Gumbel distributions and check for the relative validity of the generated probabilities" in {
    //model the distribution of the maximum (or the minimum) of a number of samples of various distributions.
    val seq1 = PdfStreamGenerator(Constants.GumbelDistribution, Array(0,5, 2)).take(100).toList
    val seq2 = PdfStreamGenerator(Constants.GumbelDistribution, Array(10, 5)).take(100).toList
    seq1.sum / 100 should be < seq2.sum / 100
  }

  it should "create two hypergeometric distributions and check for the relative validity of the generated probabilities" in {
    //model the distribution of the maximum (or the minimum) of a number of samples of various distributions.
    //the hypergeometric distribution is a discrete probability distribution that describes the probability of k successes
    // (random draws for which the object drawn has a specified feature) in n draws, without replacement, from a finite
    // population of size N that contains exactly K objects with that feature, wherein each draw is either a success or a failure.
    // In contrast, the binomial distribution describes the probability of k successes in n draws with replacement.
    //populationSize: Int, numberOfSuccesses: Int, sampleSize: Int
    val seq1 = PdfStreamGenerator(Constants.HypergeometricDistribution, Array(500, 50, 100)).take(100).toList
    val seq2 = PdfStreamGenerator(Constants.HypergeometricDistribution, Array(900, 100, 200)).take(100).toList
    seq1.sum / 100 should be < seq2.sum / 100
  }

  it should "create two laplace distributions and check for the relative validity of the generated probabilities" in {
    //The difference between two independent identically distributed exponential random variables is governed by a Laplace distribution, as is a Brownian motion evaluated at an exponentially distributed random time. I
    val seq1 = PdfStreamGenerator(Constants.LaplaceDistribution, Array(1, 2)).take(100).toList
    val seq2 = PdfStreamGenerator(Constants.LaplaceDistribution, Array(10, 1)).take(100).toList
    seq1.sum / 100 should be < seq2.sum / 100
  }

  it should "create two levy distributions and check for the relative validity of the generated probabilities" in {
    val seq1 = PdfStreamGenerator(Constants.LevyDistribution, Array(0.1, 2)).take(100).toList
    val seq2 = PdfStreamGenerator(Constants.LevyDistribution, Array(5, 10)).take(100).toList
    seq1.sum / 100 should be < seq2.sum / 100
  }

  it should "create two normal distributions and check for the relative validity of the generated probabilities" in {
    val seq1 = PdfStreamGenerator(Constants.NormalDistribution, Array(10, 2)).take(100).toList
    val seq2 = PdfStreamGenerator(Constants.NormalDistribution, Array(50, 10)).take(100).toList
    seq1.sum / 100 should be < seq2.sum / 100
  }

  it should "create two lognormal distributions and check for the relative validity of the generated probabilities" in {
    val seq1 = PdfStreamGenerator(Constants.LogNormalDistribution, Array(10, 2)).take(100).toList
    val seq2 = PdfStreamGenerator(Constants.LogNormalDistribution, Array(50, 10)).take(100).toList
    seq1.sum / 100 should be < seq2.sum / 100
  }

  it should "create two logistic distributions and check for the relative validity of the generated probabilities" in {
    val seq1 = PdfStreamGenerator(Constants.LogisticDistribution, Array(10, 2)).take(100).toList
    val seq2 = PdfStreamGenerator(Constants.LogisticDistribution, Array(50, 10)).take(100).toList
    seq1.sum / 100 should be < seq2.sum / 100
  }

  it should "create two nakagami distributions and check for the relative validity of the generated probabilities" in {
    val seq1 = PdfStreamGenerator(Constants.NakagamiDistribution, Array(1, 2)).take(100).toList
    val seq2 = PdfStreamGenerator(Constants.NakagamiDistribution, Array(1, 5)).take(100).toList
    seq1.sum / 100 should be < seq2.sum / 100
  }

  it should "create two pareto distributions and check for the relative validity of the generated probabilities" in {
    val seq1 = PdfStreamGenerator(Constants.ParetoDistribution, Array(1000, 1)).take(100).toList
    val seq2 = PdfStreamGenerator(Constants.ParetoDistribution, Array(1, 1)).take(100).toList
    seq1.sum / 100 should be > seq2.sum / 100
  }

  it should "create two pascal distributions and check for the relative validity of the generated probabilities" in {
    //r > 0 — number of failures until the experiment is stopped (integer, but the definition can also be extended to reals)
    //p ∈ [0,1] — success probability in each experiment (real)
    val seq1 = PdfStreamGenerator(Constants.PascalDistribution, Array(10, 0.2)).take(100).toList
    val seq2 = PdfStreamGenerator(Constants.PascalDistribution, Array(100, 0.8)).take(100).toList
    seq1.sum / 100 should be > seq2.sum / 100
  }

//  val wrongProbabilityValue = 1.2
//  the[Throwable] thrownBy PdfStreamGenerator(Constants.PascalDistribution, Array(10, wrongProbabilityValue)) 

  it should "create two poisson distributions and check for the relative validity of the generated probabilities" in {
    //expresses the probability of a given number of events occurring in a fixed interval of time or space if these events occur with a known constant mean rate and independently of the time since the last event.
    val seq1 = PdfStreamGenerator(Constants.PoissonDistribution, Array(5)).take(100).toList
    val seq2 = PdfStreamGenerator(Constants.PoissonDistribution, Array(100)).take(100).toList
    seq1.sum / 100 should be < seq2.sum / 100
  }

  it should "create two T distributions and check for the relative validity of the generated probabilities" in {
    //it is more prone to producing values that fall far from its mean.
    val seq1 = PdfStreamGenerator(Constants.TDistribution, Array(5)).take(100).toList
    val seq2 = PdfStreamGenerator(Constants.TDistribution, Array(1000)).take(100).toList
    seq1.sum / 100 should be > seq2.sum / 100
  }

  it should "create two Triangular distributions and check for the relative validity of the generated probabilities" in {
    val seq1 = PdfStreamGenerator(Constants.TriangularDistribution, Array(5, 200, 3000)).take(100).toList
    val seq2 = PdfStreamGenerator(Constants.TriangularDistribution, Array(1, 10, 100)).take(100).toList
    seq1.sum / 100 should be > seq2.sum / 100
  }

//  the[Throwable] thrownBy PdfStreamGenerator(Constants.TriangularDistribution, Array(-2, 10, 8))

  it should "create two uniform real value distributions and check for the relative validity of the generated probabilities" in {
    val seq1 = PdfStreamGenerator(Constants.UniformRealDistribution, Array(-5, 5)).take(100).toList
    val seq2 = PdfStreamGenerator(Constants.UniformRealDistribution, Array(1, 10)).take(100).toList
    seq1.sum / 100 should be < seq2.sum / 100
  }

  it should "create two uniform int value distributions and check for the relative validity of the generated probabilities" in {
    val seq1 = PdfStreamGenerator(Constants.UniformIntegerDistribution, Array(-5, 5)).take(100).toList
    val seq2 = PdfStreamGenerator(Constants.UniformIntegerDistribution, Array(1, 10)).take(100).toList
    seq1.sum / 100 should be < seq2.sum / 100
  }

  it should "create two weibull distributions and check for the relative validity of the generated probabilities" in {
    val seq1 = PdfStreamGenerator(Constants.WeibullDistribution, Array(1, 0.5)).take(100).toList
    val seq2 = PdfStreamGenerator(Constants.WeibullDistribution, Array(1, 10)).take(100).toList
    seq1.sum / 100 should be < seq2.sum / 100
  }

  it should "create two zipf distributions and check for the relative validity of the generated probabilities" in {
    //number of elements and exponent
    val seq1 = PdfStreamGenerator(Constants.ZipfDistribution, Array(1000, 0.5)).take(100).toList
    val seq2 = PdfStreamGenerator(Constants.ZipfDistribution, Array(1000, 1)).take(100).toList
    seq1.sum / 100 should be > seq2.sum / 100
  }

  it should "create two enumerated distributions and check for the relative validity of the generated probabilities" in {
    //number of elements and exponent
    val seq1 = PdfStreamGenerator(Constants.EnumeratedIntegerDistribution, Array(0.1, 0.1, 0.5, 0.1, 0.1, 0.05, 0.05), Array(1, 2, 3, 4, 5, 6, 7)).take(100).toList
    val seq2 = PdfStreamGenerator(Constants.EnumeratedIntegerDistribution, Array(0.5, 0.1, 0.1, 0.1, 0.1, 0.05, 0.05), Array(1, 2, 3, 4, 5, 6, 7)).take(100).toList
    seq1.sum / 100 should be > seq2.sum / 100
  }
}
