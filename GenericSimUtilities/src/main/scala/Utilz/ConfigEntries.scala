/*
 * Copyright (c) 7/3/24, 12:19 PM, 3. Mark Grechanik and Lone Star Consulting, Inc. All rights reserved.
 *  
 *  Unless required by applicable law or agreed to in writing, software distributed under the License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  
 *  See the License for the specific language governing permissions and limitations under the License.
 */

package Utilz

/*
DIALS {
  General {
    verbose = true
    outputDirectory = "/path"
    debugMode = true
  }
  Random {
    #ISAACRandom,  JDKRandomGenerator, MersenneTwister, Well512a, Well1024a, Well19937a, Well19937c, Well44497a, Well44497b
    generator = Well44497a
    seed = 7128936543
    #     beta, binomial, cauchy, chisquared, exponential, f, gamma, geometric, gumbel, hypergeometric, laplace, levy, lognormal, logistic, nakagami, normal,
    #     pareto, pascal, poisson, t, triangular, uniformreal, uniforminteger, weibull, zipf, enumint
  }
  Analyses {
    generateGraph = true
    checkReachability = true
    checkStateMachines = true
    generateResourceMap = true
    generateAgentMessageMap = true
    generateMessageGraph = true
    checkControlDataFlows = true
    outputOrphanedEntities = true
  }
  Runtime {
    simulationTimeStepMilliSeconds = 100
    vectorClock = true
    consistency = eventual
  }
*/


trait ConfigEntries:
  val `DIALS.General.verbose`: Boolean
  val `DIALS.General.outputDirectory`: String
  val `DIALS.General.debugMode`: Boolean
  val `DIALS.Random.generator`: String
  val `DIALS.Random.seed`: Long
  val `DIALS.Analyses.generateGraph`: Boolean
  val `DIALS.Analyses.checkReachability`: Boolean
  val `DIALS.Analyses.checkStateMachines`: Boolean
  val `DIALS.Analyses.generateResourceMap`: Boolean
  val `DIALS.Analyses.generateAgentMessageMap`: Boolean
  val `DIALS.Analyses.generateMessageGraph`: Boolean
  val `DIALS.Analyses.checkControlDataFlows`: Boolean
  val `DIALS.Analyses.outputOrphanedEntities`: Boolean
  val `DIALS.Runtime.simulationTimeStepMilliSeconds`: Int
  val `DIALS.Runtime.vectorClock`: Boolean
  val `DIALS.Runtime.consistency`: String
