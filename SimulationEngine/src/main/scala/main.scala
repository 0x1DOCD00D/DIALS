import GenericDefinitions.AkkaMessages.*
import GenericDefinitions.*
import akka.actor.{Actor, ActorRef, ActorSystem, Props}

// ───────────────────────────────────────────────────────────────
// 1. Base actor marker
// ───────────────────────────────────────────────────────────────
trait DialsActor extends Actor

// ───────────────────────────────────────────────────────────────
// 2. AgentActor
// ───────────────────────────────────────────────────────────────
class AgentActor(name: String, entity: AgentEntity) extends DialsActor {

  // each actor owns one implicit ctx
  private given ctx: ProcessingContext = ProcessingContext(self, "Agent", executionFlag = true)

  def receive: Receive = uninitialized

  private def uninitialized: Receive = {
    case Setup(resRefs, chanRefs) =>


      println(ctx.resources)


      val initState = entity.getAutoTriggeredState
        .orElse(entity.getStates.headOption)
        .getOrElse(throw new IllegalStateException("No initial state"))

      val localRes = spawnLocalResources()
      ProcessingContext.withCtx(ctx) {
        Ctx.setChannels(chanRefs);
        Ctx.setFlag(true);
        Ctx.setResources(localRes ++ resRefs)
      }

      context.become(running(entity, resRefs ++ localRes, chanRefs, Some(initState.name)))
  }

  private def spawnLocalResources(): Map[String,ActorRef] =
    entity.getResources.map(r => r.name -> context.actorOf(ResourceActor.props(r.name,r), r.name)).toMap



  private def running(ent: AgentEntity,
                      resources: Map[String,ActorRef],
                      channels:  Map[String,ActorRef],
                      active:    Option[String]): Receive = {

    println(s"[$name] running state=${active.getOrElse("<none>")}")
    println(s"[$name] resources=${resources.keys}")
    println(s"[$name] channels =${channels.keys}")

    val state = ent.getStates.find(_.name == active.getOrElse(""))
      .getOrElse(throw new IllegalStateException("No active state"))

    ProcessingContext.withCtx(ctx) {
      println(Ctx.toString)
      state.onSwitchBehavior.foreach(_.apply)
      state.behaviors.flatMap(_.onActiveActions).foreach(f => f(ctx))
    }

//    TODO: Set scheduler to transition to timeout state if applicable


    { case msg : ChannelMessage =>
        val messageEntity: MessageEntity = msg.dialsMessage
        val sender = msg.sender
//        TODO: GenericMessageTemplate

        try {
          val messageMatch = GenericMessageTemplate(messageEntity.name, messageEntity.values.toList.collect { case d: Double => d }, None)

          ProcessingContext.withCtx(ctx) {
            val msgMatch = state.behaviors.flatMap(_.actualActions).find(_(ctx).isDefinedAt(messageMatch))
            println(s"[$name] received message ${messageEntity.name} from ${sender.path.name}")
            msgMatch.foreach(_(ctx).apply(messageMatch))
          }
        }
        catch {
          case e: Exception =>
            println(s"[$name] error in message match ${e.getMessage}")
            val messageMatch = GenericMessageTemplate(messageEntity.name, List.empty , None)
            ProcessingContext.withCtx(ctx) {
              val msgMatch = state.behaviors.flatMap(_.actualActions).find(_(ctx).isDefinedAt(messageMatch))
              println(s"[$name] received message ${messageEntity.name} from ${sender.path.name}")
              msgMatch.foreach(_(ctx).apply(messageMatch))
            }
        }

//        TODO: transition to next state from StateTransitions of the entity based on executing conditions (withCtx)



      case msg1=>
        println(s"[$name] unknown message $msg1")
    }
  }
}
object AgentActor { def props(n:String,e:AgentEntity):Props = Props(new AgentActor(n,e)) }

// ───────────────────────────────────────────────────────────────
// 3. ChannelActor
// ───────────────────────────────────────────────────────────────
class ChannelActor(name: String, from: ActorRef, to: ActorRef, dir: ModelEntity.DIRECTION)
  extends DialsActor {

  private given ctx: ProcessingContext = ProcessingContext(self,"Channel", executionFlag=true)

  def receive: Receive = {
    case msg: ChannelMessage =>
      println(s"[$name] Channel relays ${msg.dialsMessage}")
      dir match
        case ModelEntity.DIRECTION.LEFT2RIGHT  => relay(msg, from, to)
        case ModelEntity.DIRECTION.RIGHT2LEFT  => relay(msg, to, from)
        case ModelEntity.DIRECTION.BIDIRECTIONAL =>
          if      msg.sender == from then to ! msg
          else if msg.sender == to   then from ! msg
    case other =>
      println(s"[$name] unknown $other")
  }

  private def relay(m: ChannelMessage, check: ActorRef, send: ActorRef): Unit =
    if m.sender == check then send ! m
    else println(s"[$name] wrong sender ${m.sender}")
}
object ChannelActor { def props(n:String,f:ActorRef,t:ActorRef,d:ModelEntity.DIRECTION):Props =
  Props(new ChannelActor(n,f,t,d)) }

// ───────────────────────────────────────────────────────────────
// 4. ResourceActor
// ───────────────────────────────────────────────────────────────
class ResourceActor(name: String, entity: ResourceEntity) extends DialsActor {

  private given ctx: ProcessingContext = ProcessingContext(self, "Resource" ,executionFlag=true)

  def receive: Receive = {
    case ResourceSetMessage(_, r, v) if r == entity.name =>
      println(s"[$name] set resource ${entity.name} to $v")
      entity set v
    case ResourceGetMessage(sen, r) if r == entity.name =>
      println(s"[$name] get resource ${entity.name}")
      sender() ! ResourceGetResponse(self, r, entity.getValues)
    case other => println(s"[$name] unknown $other")
  }
}
object ResourceActor { def props(n:String,e:ResourceEntity):Props = Props(new ResourceActor(n,e)) }

// ───────────────────────────────────────────────────────────────
// 5. Akka‑system bootstrap (unchanged logic)
// ───────────────────────────────────────────────────────────────
def setupAkkaSystem(): ActorSystem = ActorSystem("DialsSystem")

def extractEntityName(e: ModelGraphNode): String = e match
  case EntityInstanceAlias(a, _) => a
  case ag: AgentEntity           => ag.name
  case _                         => throw IllegalArgumentException("Unknown node")

// === 6. Main Application ===
@main
def main(): Unit = {
  val (model, validationRes) = PingPongModel.buildModel()

  val system = setupAkkaSystem()

  println(model)

  val globalResource = ResourceEntity.getTopLevelResources
  val fromAgents = model.connections.map(_.asInstanceOf[CompletedChain].from._1)
  val toAgents = model.connections.map(_.asInstanceOf[CompletedChain].to._1)

  val agentSet = (fromAgents ++ toAgents).toSet
  //  if entityalias is present extract internal agent and entity name
  // else just agent and name
  val agentNameMap: Map[String, AgentEntity] = agentSet.map {
    case agent: AgentEntity =>
      agent.name -> agent
    case EntityInstanceAlias(alias, Some(ent: AgentEntity)) =>
      alias -> ent
  }.toMap

  type ChannelWDirection = (ChannelEntity, (ModelGraphNode, ModelGraphNode, ModelEntity.DIRECTION))

  val channelWDirection = model.connections.map {
    case CompletedChain(from, to, channel, direction) =>
      (channel.asInstanceOf[ChannelEntity], (from, to, direction))
  }

  val agentActors = agentNameMap.map {
    case (name, agent) =>
      val actorRef = system.actorOf(AgentActor.props(name, agent), name)
      name -> actorRef
  }

  val channelActors = channelWDirection.map {
    case (channel, (from, to, direction)) =>

      val fromAgent = agentActors(extractEntityName(from._1))
      val toAgent = agentActors(extractEntityName(to._1))
      val actorRef = system.actorOf(Props(new ChannelActor(channel.name, fromAgent, toAgent, direction)), channel.name)
      channel.name -> actorRef
  }.toMap

  val globalResourceActors = globalResource.map {
    case resource: ResourceEntity =>
      val actorRef = system.actorOf(ResourceActor.props(resource.name, resource), resource.name)
      resource.name -> actorRef
  }
  // === 1. Convert global resources into a single map
  val allResourceRefs: Map[String, ActorRef] = globalResourceActors.toMap

  // Map every agent → list of channel ActorRefs it is connected to
  val agentToChannels: Map[String, List[ActorRef]] =
    model.connections.flatMap {
        case CompletedChain((fromNode, _), (toNode, _), chanEnt, _) =>
          val chanRef = channelActors(chanEnt.name)
          val from = extractEntityName(fromNode)
          val to = extractEntityName(toNode)
          Seq(from -> chanRef, to -> chanRef)
      }
      .groupBy(_._1)
      .view
      .mapValues(_.map(_._2).distinct.toList)
      .toMap

  // --- Send Setup ----------------------------------------------------------
  agentActors.foreach { case (agentName, agentRef) =>
    val chanMap: Map[String, ActorRef] =
      agentToChannels.getOrElse(agentName, Nil).map(r => r.path.name -> r).toMap

    agentRef ! Setup(allResourceRefs, chanMap)
  }

  // Optional: Shutdown after use
//  system.terminate()
}

