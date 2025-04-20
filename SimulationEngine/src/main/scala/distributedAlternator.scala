import akka.actor._
import scala.concurrent.duration._
import scala.util.Random

// ───────────────────────── Messages (dispatch) ─────────────────────────
sealed trait ControlAction
case object AskPermission extends ControlAction
case object NoWayMyTurn   extends ControlAction
case object Goahead       extends ControlAction      // ⚠ keep spelling

sealed trait Data
final case class InformSinkProcess(from: String) extends Data

private case object TickRandomWait            // internal timers
private case object TimeoutWait4Responses
private case object RestartRandomWait

// ───────────────────────── MessageSinkProcess ─────────────────────────
class MessageSinkProcess extends Actor {
  // resource messageCount
  private var messageCount = 0

  def receive: Receive = {
    case InformSinkProcess(id) =>
      messageCount += 1
      println(s"[Sink] received from $id – total messages = $messageCount")
  }
}

// ───────────────────────── AlternatorProcess ─────────────────────────
class AlternatorProcess(
                         val processID: String,
                         var neighbors: Seq[ActorRef],          // wired after creation
                         sink: ActorRef)
  extends Actor with Timers {

  // ───── resources ─────
  private val random   = new Random()
  private var randomWait: FiniteDuration = randomWaitDuration
  private var numberOfNeighbors          = neighbors.size
  private var sentNotification           = false
  private var responseCount              = 0
  private var responses                  = Vector.empty[ControlAction]

  // ───── state machine entry ─────
  override def preStart(): Unit =
    self ! RestartRandomWait                       // enter state randomWait immediately

  // ───────────────── STATE randomWait ─────────────────
  def randomWaitState: Receive = {
    case RestartRandomWait =>
      responseCount     = 0
      sentNotification  = false
      responses         = Vector.empty
      randomWait        = randomWaitDuration
      timers.startSingleTimer(TickRandomWait, TickRandomWait, randomWait)

    case TickRandomWait =>
      context.become(contactNeighborsState)
      self ! KickedOffContactNeighbors              // synthetic trigger

    case AskPermission =>
      // neighbour wants in – grant immediately
      sender() ! Goahead
  }

  private case object KickedOffContactNeighbors

  // ───────────────── STATE ContactNeighbors ─────────────────
  def contactNeighborsState: Receive = {
    case KickedOffContactNeighbors =>
      neighbors.foreach(_ ! AskPermission)
      sentNotification = true
      timers.startSingleTimer(TimeoutWait4Responses, TimeoutWait4Responses, 3.seconds)
      context.become(wait4ResponsesFromNeighborsState(Set(neighbors: _*)))

    case AskPermission =>
      sender() ! NoWayMyTurn                       // I'm about to enter CS
  }

  // ───────────────── STATE Wait4ResponsesFromNeighbors ───────────────
  def wait4ResponsesFromNeighborsState(pending: Set[ActorRef]): Receive = {
    case Goahead if pending.contains(sender()) =>
      val nextPending = pending - sender()
      responses      :+= Goahead
      responseCount  += 1
      if (nextPending.isEmpty) enterProceedState()
      else context.become(wait4ResponsesFromNeighborsState(nextPending))

    case NoWayMyTurn =>
      // neighbour is busy – back to random wait
      enterRandomWait()

    case AskPermission =>
      sender() ! NoWayMyTurn                       // still waiting myself

    case TimeoutWait4Responses =>
      // timeout – back to random wait
      enterRandomWait()
  }

  // ───────────────── STATE Proceed ─────────────────
  def proceedState: Receive = {
    case "enter" =>                                // synthetic
      sink ! InformSinkProcess(processID)
      sentNotification = true
      timers.startSingleTimer(RestartRandomWait, RestartRandomWait, 3.seconds)

    case RestartRandomWait =>
      enterRandomWait()

    case AskPermission =>                          // deny while in CS
      sender() ! NoWayMyTurn
  }

  // ───── helpers to switch states ─────
  private def enterRandomWait(): Unit = {
    context.become(randomWaitState)
    self ! RestartRandomWait
  }
  private def enterProceedState(): Unit = {
    context.become(proceedState)
    self ! "enter"
  }
  private def randomWaitDuration: FiniteDuration =
    (5 + random.nextInt(6)).seconds                // UniformIntegerDistribution(5,10)

  // initial behavior
  def receive: Receive = randomWaitState
}

// ───────────────────────── Model distributedAlternator ─────────────────
object distributedAlternator extends App {
  val system = ActorSystem("distributedAlternator")
  import system.dispatcher

  val sink = system.actorOf(Props[MessageSinkProcess](), "MessageSinkProcess")

  // Create four alternators first, neighbours wired later
  val A = system.actorOf(Props(classOf[AlternatorProcess], "A", Seq.empty[ActorRef], sink), "A")
  val B = system.actorOf(Props(classOf[AlternatorProcess], "B", Seq.empty[ActorRef], sink), "B")
  val C = system.actorOf(Props(classOf[AlternatorProcess], "C", Seq.empty[ActorRef], sink), "C")
  val D = system.actorOf(Props(classOf[AlternatorProcess], "D", Seq.empty[ActorRef], sink), "D")

  // Ring: A ↔ B ↔ C ↔ D ↔ A
  val ring = Seq(A, B, C, D)
  def neigh(i: Int) = Seq(ring((i + 1) % 4), ring((i + 3) % 4))

  // mutate neighbours (safe because actors not yet started heavy work)
  ring.zipWithIndex.foreach { case (ref, idx) =>
    ref ! PoisonPill                                      // stop temp
  }

  // Re‑create with proper neighbours (simplest trick for illustration):
  val altRefs = Vector.tabulate(4) { i =>
    system.actorOf(
      Props(classOf[AlternatorProcess], ('A' + i).toChar.toString, neigh(i), sink),
      s"AlternatorProcess-${'A' + i}"
    )
  }

  // Let simulation run 30 s then shut down
  system.scheduler.scheduleOnce(30.seconds)(system.terminate())
}
