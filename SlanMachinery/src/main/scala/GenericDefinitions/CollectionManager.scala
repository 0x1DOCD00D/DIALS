/*
 * Copyright (c) 7/27/24, 3:11 PM, 27. Mark Grechanik and Lone Star Consulting, Inc. All rights reserved.
 *  
 *  Unless required by applicable law or agreed to in writing, software distributed under the License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  
 *  See the License for the specific language governing permissions and limitations under the License.
 */

package GenericDefinitions

import Pdfs.PdfStreamGenerator
import Utilz.{ConfigDb, Constants}

object CollectionManager:
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
            PdfStreamGenerator(Constants.UniformIntegerDistribution, Array(from.toDouble, to.toDouble)).take(1).toList.head.toInt
        if v <= 0 then List.empty else (0 to v-1).toList
    
