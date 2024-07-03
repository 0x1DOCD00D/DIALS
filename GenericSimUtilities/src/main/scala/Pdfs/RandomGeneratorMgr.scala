/*
 * Copyright (c) 6/2/24, 2:00 PM, 2. Mark Grechanik and Lone Star Consulting, Inc. All rights reserved.
 *  
 *  Unless required by applicable law or agreed to in writing, software distributed under the License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  
 *  See the License for the specific language governing permissions and limitations under the License.
 */

package Pdfs

import Utilz.{ConfigDb, Constants}
import com.google.common.reflect.ClassPath

import scala.util.{Failure, Success, Try}
import org.apache.commons.math3.random.*

import java.util.stream.Collectors
import scala.jdk.CollectionConverters.*

object RandomGeneratorMgr:
  def apply(): RandomGenerator =
    val clz: RandomGenerator = ClassPath
      .from(ClassLoader.getSystemClassLoader)
      .getAllClasses
      .asScala
      .toList
      .filter(c => c.getPackageName.equalsIgnoreCase(Constants.MathApacheRandomPkgName))
      .filter(c => c.getSimpleName.equalsIgnoreCase(ConfigDb.`DIALS.Random.generator`)).headOption match
      case Some(clzzRes) => Try(clzzRes.load()) match
        case Failure(exception) => throw new IllegalArgumentException(s"Class ${ConfigDb.`DIALS.Random.generator`} cannot be loaded from package ${Constants.MathApacheRandomPkgName}")
        case Success(clzz) => clzz.getDeclaredConstructor().newInstance().asInstanceOf[RandomGenerator]
      case None => throw new IllegalArgumentException(s"Class ${ConfigDb.`DIALS.Random.generator`} not found in package ${Constants.MathApacheRandomPkgName}")
    clz.setSeed(ConfigDb.`DIALS.Random.seed`)
    clz
