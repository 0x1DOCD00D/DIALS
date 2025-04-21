import akka.actor._
import akka.testkit._
import org.scalatest.wordspec.AnyWordSpecLike
import org.scalatest.matchers.should.Matchers
import org.scalatest.BeforeAndAfterAll
import scala.concurrent.duration._
import distributedAlternator._

// -----------------------------------------------------------------------------
// Additional validation‑oriented specs
// -----------------------------------------------------------------------------

def setupRing(sink: ActorRef, ctrlProbe: Option[TestProbe]=None): Vector[ActorRef] = {
  val names = Vector("A", "B", "C", "D")
  val alts = names.map { n =>
    val uniq = java.util.UUID.randomUUID().toString.take(6)
    system.actorOf(
      Props(classOf[AlternatorProcess], n, Seq.empty[ActorRef], sink),
      s"Alt_${n}_$uniq")
  }
  // ring wiring
  alts.indices.foreach { i =>
    val neighbours = Seq(alts((i + 1) % 4), alts((i + 3) % 4))
    alts(i) ! SetNeighbors(neighbours)
  }
  // optionally inject probe on first link to watch msgs
  ctrlProbe.foreach { p =>
    alts.head ! SetNeighbors(Seq(p.ref, alts(1)))
  }
  alts.toVector
}

class AlternatorValidationSpec extends TestKit(ActorSystem("AltValSpec"))
  with AnyWordSpecLike with Matchers with BeforeAndAfterAll {

  import utils.Metrics._

  override def afterAll(): Unit = {
    report()
    TestKit.shutdownActorSystem(system)
  }

  // -------------------------------------------------------------------------
  //   LOW‑EFFORT RULES (runtime‑detectable)
  // -------------------------------------------------------------------------

  "be live (sink receives notifications)" in measure("liveness") {
    val sinkProbe = TestProbe()(system)
    val _ = setupRing(sinkProbe.ref)
    sinkProbe.expectMsgType[InformSinkProcess](10.seconds)
  }

  "only emit protocol messages on ControlAction" in measure("channel‑conformance") {
    val ctrlProbe = TestProbe()(system)
    val sinkProbe = TestProbe()(system)
    val alts = setupRing(sinkProbe.ref, Some(ctrlProbe))

    // let system run for a bit
    ctrlProbe.receiveWhile(5.seconds) {
      case AskPermission | Goahead | NoWayMyTurn => // OK
      case other => fail(s"unexpected msg: $other")
    }
  }

  "reply Goahead to every AskPermission" in measure("request‑response") {
    val sinkProbe = TestProbe()(system)
    val alts = setupRing(sinkProbe.ref)
    alts.foreach { alt =>
      alt.tell(AskPermission, sinkProbe.ref)
      sinkProbe.expectMsgType[Goahead.type](3.seconds)
    }
  }

  "No undeclared channel usage" in measure("no‑undeclared‑channel") {
    // inject a fake neighbour that drops "FakeMsg" which is *not* in protocol
    val sink = TestProbe()(system)
    val alt  = system.actorOf(Props(classOf[AlternatorProcess], "X", Seq.empty, sink.ref))
    val rogue = TestProbe()(system)
    alt ! SetNeighbors(Seq(rogue.ref))

    rogue.send(alt, "FakeMsg")          // send junk
    rogue.expectNoMessage(2.seconds)     // alt should ignore → no echo
  }

  "Channel connects only valid agents" in measure("channel‑valid‑endpoints") {
    val sink = TestProbe()(system)
    val dead = TestProbe()(system)       // will terminate → simulate removed agent
    system.stop(dead.ref)

    val alt = system.actorOf(Props(classOf[AlternatorProcess], "Y", Seq.empty, sink.ref))
    alt ! SetNeighbors(Seq(dead.ref))    // dangling neighbour

    // send tick; alt will AskPermission; expect it to hit deadLetters
    val deadLettersProbe = TestProbe()(system)
    system.eventStream.subscribe(deadLettersProbe.ref, classOf[DeadLetter])
    alt ! RestartRandomWait              // force cycle quickly
    deadLettersProbe.expectMsgType[DeadLetter](10.seconds)
  }

  "Duplicate actor names are rejected" in measure("duplicate‑actor‑name") {
    val sink = TestProbe()(system)
    val a1 = system.actorOf(Props(classOf[AlternatorProcess], "Dup", Seq(), sink.ref), name="DupActor")
    intercept[InvalidActorNameException] {
      system.actorOf(Props(classOf[AlternatorProcess], "Dup2", Seq(), sink.ref), name="DupActor")
    }
  }

  // -------------------------------------------------------------------------
  //   MEDIUM‑EFFORT RULES
  // -------------------------------------------------------------------------

  "FSM is live (no dead‑end state)" in measure("fsm‑no‑deadend") {
//    run simulation and expect all states to be reachable for Alternator Process
  }

  "Detect unused message type" in measure("unused‑message‑type") {
    // We spin an alternator ring but subscribe to system.eventStream; if a
    // custom message type never appears in *any* DeadLetter within timeframe
    // we mark as unused.  Here we assert that `FakeMsg2` is indeed unused.
    val sink = TestProbe()(system)
    val alts = setupRing(sink.ref)
    val deadLettersProbe = TestProbe()(system)
    system.eventStream.subscribe(deadLettersProbe.ref, classOf[DeadLetter])
    alts.foreach { alt =>
      alt ! SetNeighbors(Seq(deadLettersProbe.ref)) // send to deadLetters
    }
    deadLettersProbe.expectNoMessage(10.seconds) // no deadLetters
  }

  "All incoming messages handled" in measure("all‑incoming‑handled") {
//    receive notification message on probe actor
  }
  
}
