/*
 * Copyright (newConnection) 7/16/24, 9:22 AM, 16. Mark Grechanik and Lone Star Consulting, Inc. All rights reserved.
 *
 *  Unless required by applicable law or agreed to in writing, software distributed under the License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *
 *  See the License for the specific language governing permissions and limitations under the License.
 */

package GenericDefinitions

import org.scalatest.funsuite.AnyFunSuiteLike
import Utilz.{Constants, CreateLogger}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.slf4j.Logger

import scala.Dynamic
import scala.language.dynamics
import scala.language.postfixOps
import Keywords.*
import org.scalatest.DoNotDiscover

@DoNotDiscover
class ModelEntityTest extends AnyFlatSpec with Matchers {
  val logger: Logger = CreateLogger(classOf[ModelEntityTest])
  behavior of "model entities"

  it should "generate a model definition with three connected agents" in {
    (model m1) `is defined as` {
      (agent a1) <~> (channel c1) <~> (agent a2);
      (agent a2) ~> (channel c2) ~> (agent a3);
      (agent a3) <~ (channel c3) <~ (agent a1)
    }
    logger.info(ModelEntity.toString)
    ModelEntity().map(_.name) shouldBe List("m1")
    ModelEntity.resetAll()
  }

  it should "generate a model definition with partial connections" in {
    (model m2) `is defined as` {
      (agent a1) <~> (channel c1) <~> (agent a2);
      (agent a2) ~> (channel c2);
      (agent a3) <~ (channel c3) <~ (agent a1)
    }
    logger.info(ModelEntity.toString)
    val modelList = ModelEntity()
    modelList.head.connections.filter(_.isInstanceOf[PartialConnection]).size shouldBe 1
    modelList.head.connections.filter(_.isInstanceOf[CompleteConnection]).size shouldBe 2
    modelList.head.connections.size shouldBe 3
    modelList.map(_.name) shouldBe List("m2")
    ModelEntity.resetAll()
  }

  it should "generate a model definition with one partial connection and two extended connections" in {
    (model m2) `is defined as` {
      (agent a1) <~> (channel c1) <~> (agent a2) <~> (channel c4) <~> (agent a3);
      (agent a1) <~> (channel c1) <~> (agent a2) <~ (channel c4) <~ (agent a3);
      (agent a2) ~> (channel c2);
      (agent a3) <~ (channel c3) <~ (agent a1)
    }
    logger.info(ModelEntity.toString)
    val modelList = ModelEntity()
    modelList.head.connections.filter(_.isInstanceOf[PartialConnection]).size shouldBe 1
    modelList.head.connections.filter(_.isInstanceOf[CompleteConnection]).size shouldBe 5
    modelList.head.connections.size shouldBe 6
    modelList.map(_.name) shouldBe List("m2")
    ModelEntity.resetAll()
    AgentEntity.resetAll()
    ChannelEntity.resetAll()
  }

  it should "generate a model definition with one partial connection and three extended connections" in {
    (model m2) `is defined as` {
      (agent a1) <~> (channel c1) <~> (agent a2) <~> (channel c4) <~> (agent a3);
      ((agent a1) <~> (channel c1) <~> (agent a2)) ~> (channel c4) ~> (agent a3);
      (agent a1) <~> (channel c1) <~> (agent a2) <~ (channel c4) <~ (agent a3);
      (agent a2) ~> (channel c2);
      (agent a3) <~ (channel c3) <~ (agent a1)
    }
    logger.info(ModelEntity.toString)
    val modelList = ModelEntity()
    modelList.head.connections.filter(_.isInstanceOf[PartialConnection]).size shouldBe 1
    modelList.head.connections.filter(_.isInstanceOf[CompleteConnection]).size shouldBe 7
    modelList.head.connections.size shouldBe 8
    modelList.map(_.name) shouldBe List("m2")
    ModelEntity.resetAll()
  }

  it should "generate a model definition with a long chain" in {
    (model m3) `is defined as` {
      (((agent a1) <~> (channel c1) <~> (agent a2)) <~> (channel c4) <~> (agent a3)) ~> (channel c5) ~> (agent a4) <~ (channel c6) <~ (agent a1);
    }
    logger.info(ModelEntity.toString)
    val modelList = ModelEntity()
    modelList.head.connections.filter(_.isInstanceOf[PartialConnection]).size shouldBe 0
    modelList.head.connections.filter(_.isInstanceOf[CompleteConnection]).size shouldBe 4
    modelList.head.connections.size shouldBe 4
    modelList.map(_.name) shouldBe List("m3")
    ModelEntity.resetAll()
  }

  it should "define a model with cardinalities of entities" in {
    (model m4) `is defined as` {
      |(agent a1)| := exactly 10;
      |(agent a2)| := exactly (instance A);
      |(agent a3)| := approximately 100 `plus or minus` 10;
      |(agent a4)| := between 10 and 20;
      |(agent a5)| := exactly ((pdf NormalDistribution) as (100, 10));
      |(agent a6)| less than 10;
      |(group g1)| greater than 2;
    }
    logger.info(AgentEntity.toString)
    logger.info(ModelEntity.toString)
    val modelList = ModelEntity()
    logger.info(ModelEntity.toString)
    AgentEntity().size shouldBe 6
    GroupEntity().size shouldBe 1
    AgentEntity.getAliases().head._1 shouldBe "A"
    modelList.head.cardinalities.size shouldBe 6
    ModelEntity.resetAll()
    AgentEntity.resetAll()
    GroupEntity.resetAll()
  }

  it should "define a model with collections of entities" in {
    import CollectionManager.*
    (model m5) `is defined as` {
      |(agent a1) | := exactly 10;
      |(agent a2) | := exactly (instance A);
      |(agent a3) | := approximately 100 `plus or minus` 10;
      |(agent a4) | := between 10 and 20;
      |(agent a5) | := exactly ((pdf NormalDistribution) as(100, 10));
      |(agent a6) | less than 10;
      |(group g1) | greater than 2;
    } `is defined as` {
      (agent a1).collection.flatMap(x =>
        (agent a1).collection.flatMap(y =>
        List(x, x + 1, x + 2)
        ))
    }
    logger.info(AgentEntity.toString)
    logger.info(ModelEntity.toString)
    val modelList = ModelEntity()
    logger.info(ModelEntity.toString)
    AgentEntity().size shouldBe 6
    GroupEntity().size shouldBe 1
    AgentEntity.getAliases().head._1 shouldBe "A"
    modelList.head.cardinalities.size shouldBe 6
    ModelEntity.resetAll()
  }

  it should "define a model with one agent and the exact number of instances and check it as a collection" in {
    import CollectionManager.*
    (model m6) `is defined as` {
      |(agent a1)| := exactly 10;
    }
    logger.info(AgentEntity.toString)
    logger.info(ModelEntity.toString)
    val lst = (agent a1).collection
    lst.size shouldBe 10
    ModelEntity.resetAll()
  }

  it should "define a model with one agent and an approximate number of instances and check it as a collection" in {
    import CollectionManager.*
    (model m6) `is defined as` {
      |(agent a2) | := approximately 15 `plus or minus` 30;
    }
    logger.info(AgentEntity.toString)
    logger.info(ModelEntity.toString)
    val lst = (agent a2).collection
    lst.size should be < 20
    lst.size should be > 10
    ModelEntity.resetAll()
  }

  it should "define a model with one agent and an approximate number of instances generated from a pdf distribution and check it as a collection" in {
    import CollectionManager.*
    (model m7) `is defined as` {
      |(agent a3) | := exactly ((pdf NormalDistribution) as (200, 5));
    }
    logger.info(AgentEntity.toString)
    logger.info(ModelEntity.toString)
    val lst = (agent a3).collection
    lst.isEmpty shouldBe false
    lst.size should be > 1
    ModelEntity.resetAll()
  }

  it should "define a model with a number of connections" in {
    import CollectionManager.*
    (model m8) `is defined as` {
      |(agent a1) | := exactly 5;
      |(agent a2) | := exactly 8;
    } `is defined as` {
      (agent a1).collection.flatMap(ind1 =>
        (agent a2).collection.flatMap(ind2 =>
          List(((agent a1), ind1) <~> (channel c1) <~> ((agent a2),ind2))
        ))
    }
    logger.info(AgentEntity.toString)
    logger.info(ModelEntity.toString)
    val modelList = ModelEntity()
    modelList.head.connections.filter(_.isInstanceOf[PartialConnection]).size shouldBe 0
    modelList.head.connections.size shouldBe 40
    ModelEntity.resetAll()
  }

}
