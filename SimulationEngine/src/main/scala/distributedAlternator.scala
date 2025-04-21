import akka.actor._
import scala.concurrent.duration._
import scala.util.Random

// ─────────────────────────── Messages ───────────────────────────
sealed trait ControlAction
case object AskPermission extends ControlAction
case object NoWayMyTurn   extends ControlAction
case object Goahead       extends ControlAction

sealed trait TestMessage
case class CurrentState(state: String) extends TestMessage
case class MessageReceived(msg: String) extends TestMessage
case class StateTransition(from: String, to: String) extends TestMessage
case class MessageSent(msg: String) extends TestMessage


// external data sent to the sink
final case class InformSinkProcess(from: String) extends Serializable

// topology wiring (sent exactly once, right after all actors are started)
final case class SetNeighbors(refs: Seq[ActorRef])

// internal “tick” messages
private case object TickRandomWait
private case object TimeoutWait4Responses
private case object RestartRandomWait
private case object KickedOffContactNeighbors

// keys to cancel specific timers
private object TKey {
  case object Random
  case object Timeout
  case object Restart
}

// -------------------------------------------------------------------------
// Message‑sink with instrumentation
// -------------------------------------------------------------------------
class MessageSinkProcess(testActor: Option[ActorRef] = None) extends Actor {
  private var messageCount = 0
  private def emit(msg: TestMessage): Unit = testActor.foreach(_ ! msg)

  def receive: Receive = {
    case inf @ InformSinkProcess(id) =>
      messageCount += 1
      println(s"[Sink] received from $id – total messages = $messageCount")
      emit(MessageReceived(inf.productPrefix))          // == "InformSinkProcess"
  }
}
// -------------------------------------------------------------------------
// AlternatorProcess with instrumentation
// -------------------------------------------------------------------------
class AlternatorProcess(
                         processID: String,
                         private var neighbors: Seq[ActorRef],
                         sink: ActorRef,
                         testActor: Option[ActorRef] = None)
  extends Actor with Timers {

  // ---------- helpers ----------------------------------------------------
  private def emit(msg: TestMessage): Unit = testActor.foreach(_ ! msg)
  private def go(to: Receive, newState: String): Unit = {
    emit(StateTransition(currState, newState))
    currState = newState
    emit(CurrentState(newState))
    context.become(to)
  }
  private var currState = "randomWait"                // initial logical state
  private val random = new Random()

  // ---------- FSM states -------------------------------------------------
  override def preStart(): Unit = {
    emit(CurrentState("randomWait"))
    self ! RestartRandomWait
  }

  def randomWaitState: Receive = {
    case RestartRandomWait =>
      timers.startSingleTimer(TKey.Random, TickRandomWait, randomWaitDuration)

    case TickRandomWait =>
      go(contactNeighborsState, "contactNeighbors")
      self ! KickedOffContactNeighbors

    case AskPermission =>
      sender() ! Goahead
      emit(MessageReceived("AskPermission"))
      emit(MessageSent("Goahead"))

    case SetNeighbors(refs) => neighbors = refs
  }

  def contactNeighborsState: Receive = {
    case KickedOffContactNeighbors =>
      neighbors.foreach { n =>
        n ! AskPermission
        emit(MessageSent("AskPermission"))
      }
      timers.startSingleTimer(TKey.Timeout, TimeoutWait4Responses, 3.seconds)
      go(wait4ResponsesState(neighbors.toSet), "wait4Responses")

    case AskPermission =>
      sender() ! NoWayMyTurn
      emit(MessageReceived("AskPermission"))
      emit(MessageSent("NoWayMyTurn"))

    case m @ (Goahead | NoWayMyTurn) =>
      emit(MessageReceived(m.productPrefix))
      self.tell(m, sender())                          // re‑queue
  }

  def wait4ResponsesState(pending: Set[ActorRef]): Receive = {
    case Goahead if pending.contains(sender()) =>
      emit(MessageReceived("Goahead"))
      val rest = pending - sender()
      if (rest.isEmpty) {
        timers.cancel(TKey.Timeout)
        enterProceed()
      } else go(wait4ResponsesState(rest), "wait4Responses")   // stay

    case NoWayMyTurn | TimeoutWait4Responses =>
      emit(MessageReceived("NoWayMyTurn"))
      timers.cancel(TKey.Timeout)
      enterRandomWait()

    case AskPermission =>
      sender() ! NoWayMyTurn
      emit(MessageReceived("AskPermission"))
      emit(MessageSent("NoWayMyTurn"))
  }

  def proceedState: Receive = {
    case "enter" =>
      sink ! InformSinkProcess(processID)
      emit(MessageSent("InformSinkProcess"))
      timers.startSingleTimer(TKey.Restart, RestartRandomWait, 3.seconds)

    case RestartRandomWait =>
      timers.cancel(TKey.Restart)
      enterRandomWait()

    case AskPermission =>
      sender() ! NoWayMyTurn
      emit(MessageReceived("AskPermission"))
      emit(MessageSent("NoWayMyTurn"))
  }

  // ---------- state‑switch helpers ---------------------------------------
  private def enterRandomWait(): Unit = go(randomWaitState, "randomWait")
  private def enterProceed(): Unit     = { go(proceedState, "proceed"); self ! "enter" }
  private def randomWaitDuration: FiniteDuration =
    (5 + random.nextInt(6)).seconds

  // ---------- root behaviour ---------------------------------------------
  def receive: Receive = randomWaitState
}

// ───────────────────────── Main app ─────────────────────────
object distributedAlternator extends App {
  val system = ActorSystem("distributedAlternator")
  import system.dispatcher

  val sink     = system.actorOf(Props(classOf[MessageSinkProcess],None), "Sink")

  // create four alternators with empty neighbour lists
  val alternators = Vector.tabulate(4) { i =>
    system.actorOf(
      Props(classOf[AlternatorProcess], ('A' + i).toChar.toString, Seq.empty[ActorRef], sink, None),
      s"Alt-${('A' + i).toChar}"
    )
  }

  // wire up the ring topology: A↔B↔C↔D↔A
  alternators.indices.foreach { i =>
    val neigh = Seq(alternators((i + 1) % 4), alternators((i + 3) % 4))
    alternators(i) ! SetNeighbors(neigh)
  }

  // stop after 30 s
  system.scheduler.scheduleOnce(30.seconds) {
    system.terminate()
  }
}
