import GenericDefinitions.ResourceEntity
import akka.actor.{Actor, ActorRef, ActorSystem, Props}

// === 1. Base DIALS Actor ===
trait DialsActor extends Actor {
  // Optional common setup method or logging can go here
}

// === 2. Messages for Setup ===
object Messages {
  case class Setup(refs: Map[String, ActorRef])
}

// === 3. AgentActor ===
class AgentActor extends DialsActor {
  import Messages._

  // Mutable internal state, safe in actor
  var resources: Option[ActorRef] = None
  var channels: List[ActorRef] = List.empty

  def receive: Receive = {
    case Setup(refs) =>
      resources = refs.get("resource")
      channels = refs.get("channel").toList
    // ready for future setup
    case _ =>
    // TODO: Handle behavior
  }
}

// === 4. ChannelActor ===
class ChannelActor extends DialsActor {
  import Messages._

  var target: Option[ActorRef] = None

  def receive: Receive = {
    case Setup(refs) =>
      target = refs.get("target")
    case msg if target.isDefined =>
      target.get ! msg
    case _ =>
    // TODO: Handle undirected message
  }
}

// === 5. ResourceActor ===
class ResourceActor extends DialsActor {
  import Messages._

  var owner: Option[ActorRef] = None

  def receive: Receive = {
    case Setup(refs) =>
      owner = refs.get("agent")
    case _ =>
    // TODO: Resource handling
  }
}

def setupAkkaSystem(): ActorSystem = {
  val system = ActorSystem("DialsSystem")

  system
}


// === 6. Main Application ===
@main
def main(): Unit = {

  val (model, validationRes) = PingPongModel.buildModel()

  val system = setupAkkaSystem()




  println(model)



}