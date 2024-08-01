/*
 * Copyright (c) 7/27/24, 3:11 PM, 27. Mark Grechanik and Lone Star Consulting, Inc. All rights reserved.
 *  
 *  Unless required by applicable law or agreed to in writing, software distributed under the License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  
 *  See the License for the specific language governing permissions and limitations under the License.
 */

package GenericDefinitions

import Pdfs.PdfStreamGenerator
import Utilz.{ConfigDb, Constants, CreateLogger}

object CollectionManager:
  private val logger = CreateLogger(CollectionManager.getClass)
  extension[T <: DialsEntity] (entity: T)
    def collection: List[Int] = ModelEntity(entity) match
      case None => List.empty
      case Some(me) => 
        val v = me.value match
          case EntityPdfValue(pdf) => 
            val de = pdf.asInstanceOf[DistributionEntity]
            PdfStreamGenerator(de.name, de.params.map(_.asInstanceOf[Int].toDouble)).take(1).toList.head.toInt
          case EntityIntValue(result) => result 
          case EntityRangeOfValues(from, to) => 
            val rnd = PdfStreamGenerator(Constants.UniformIntegerDistribution, Array(from.toDouble, to.toDouble))
            val res = rnd.take(1).toList.head.toInt
            rnd.drop(1)
            res
        logger.info(s"CollectionManager: $entity has value $v")
        if v <= 0 then
          logger.error(s"CollectionManager: $entity collection size is $v which is less than or equal to 0")  
          List.empty 
        else 
          if ConfigDb.`DIALS.General.debugMode` then logger.info(s"CollectionManager: $entity collection size is $v")
          (0 to v-1).toList
    
