/*
 * Copyright (c) 6/2/24, 1:33 PM, 2. Mark Grechanik and Lone Star Consulting, Inc. All rights reserved.
 *  
 *  Unless required by applicable law or agreed to in writing, software distributed under the License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  
 *  See the License for the specific language governing permissions and limitations under the License.
 */

package Utilz

import Utilz.ConfigReader.getConfigEntry
import Utilz.{Constants, CreateLogger}
import Utilz.Constants.*
import com.typesafe.config.{Config, ConfigFactory}
import org.slf4j.Logger

import java.io.File
import java.lang.reflect.Field
import java.nio.charset.StandardCharsets
import scala.annotation.tailrec
import scala.jdk.CollectionConverters.*
import scala.util.{Failure, Random, Success, Try}

object ConfigDb extends ConfigEntries:
  import com.github.dwickern.macros.NameOf.*
  val logger: Logger = CreateLogger(classOf[ConfigDb.type])
  val `DIALS.General.verbose`: Boolean = getConfigEntry(nameOf(`DIALS.General.verbose`), false)
  val `DIALS.General.outputDirectory`: String = {
    val defDir = new java.io.File(".").getCanonicalPath
    logger.info(s"Default output directory: $defDir")
    val dir: String = getConfigEntry(nameOf(`DIALS.General.outputDirectory`), defDir)
    val ref = new File(dir)
    if ref.exists() && ref.isDirectory then
      logger.info(s"Using output directory: $dir")
      if dir.endsWith(File.separator) then dir else dir + File.separator
    else
      logger.error(s"Output directory $dir does not exist or is not a directory, using current directory instead: $defDir")
      defDir
  }
  val `DIALS.General.debugMode`: Boolean = getConfigEntry(nameOf(`DIALS.General.debugMode`), false)
  val `DIALS.Random.generator`: String = getConfigEntry(nameOf(`DIALS.Random.generator`), Constants.DefaultRandomGenerator)
  val `DIALS.Random.seed`: Long = getConfigEntry(nameOf(`DIALS.Random.seed`), Random.nextLong())
  val `DIALS.Analyses.generateGraph`: Boolean = getConfigEntry(nameOf(`DIALS.Analyses.generateGraph`), false)
  val `DIALS.Analyses.checkReachability`: Boolean = getConfigEntry(nameOf(`DIALS.Analyses.checkReachability`), false)
  val `DIALS.Analyses.checkStateMachines`: Boolean = getConfigEntry(nameOf(`DIALS.Analyses.checkStateMachines`), false)
  val `DIALS.Analyses.generateResourceMap`: Boolean = getConfigEntry(nameOf(`DIALS.Analyses.generateResourceMap`), false)
  val `DIALS.Analyses.generateAgentMessageMap`: Boolean = getConfigEntry(nameOf(`DIALS.Analyses.generateAgentMessageMap`), false)
  val `DIALS.Analyses.generateMessageGraph`: Boolean = getConfigEntry(nameOf(`DIALS.Analyses.generateMessageGraph`), false)
  val `DIALS.Analyses.checkControlDataFlows`: Boolean = getConfigEntry(nameOf(`DIALS.Analyses.checkControlDataFlows`), false)
  val `DIALS.Analyses.outputOrphanedEntities`: Boolean = getConfigEntry(nameOf(`DIALS.Analyses.outputOrphanedEntities`), false)
  val `DIALS.Runtime.simulationTimeStepMilliSeconds`: Int = getConfigEntry(nameOf(`DIALS.Runtime.simulationTimeStepMilliSeconds`), 100)
  val `DIALS.Runtime.vectorClock`: Boolean = getConfigEntry(nameOf(`DIALS.Runtime.vectorClock`), false)
  val `DIALS.Runtime.consistency`: String = getConfigEntry(nameOf(`DIALS.Runtime.consistency`), "eventual")
  
  private val checkTypeOfField: Field => Boolean = field =>
    field.getType == classOf[Boolean] ||
      field.getType == classOf[Int] ||
      field.getType == classOf[Double] ||
      field.getType == classOf[Float] ||
      field.getType == classOf[Long] ||
      field.getType == classOf[String]


  private def getFields: Map[String, String] =
    this.getClass.getDeclaredFields.filter(checkTypeOfField).map(field => field.getName.replaceAll("""\$u002E""", ".") -> field.get(this).toString).toMap[String, String]

  def apply(): Map[String, String] = getFields