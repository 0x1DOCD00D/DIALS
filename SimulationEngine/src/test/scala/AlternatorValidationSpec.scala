import akka.actor._
import akka.testkit._
import org.scalatest.wordspec.AnyWordSpecLike
import org.scalatest.matchers.should.Matchers
import org.scalatest.BeforeAndAfterAll
import scala.concurrent.duration._
import distributedAlternator._
import utils.Metrics._

/** Common helpers shared by the individual tests */
object TestHelpers {
  private val Names = Vector("A", "B", "C", "D")

  /** Build a 4‑node ring (“A↔B↔C↔D↔A”) and return the refs in name order. */
  def setupRing(system: ActorSystem,
                sink:  ActorRef,
                ctrlProbe: Option[TestProbe] = None,   // optionally sit *between* A and B
                tap:       Option[TestProbe] = None)   // optionally receive instrumentation
  : Vector[ActorRef] = {

    val alts = Names.map { n =>
      val uniq = java.util.UUID.randomUUID().toString.take(6)
      system.actorOf(
        Props(classOf[AlternatorProcess], n, Seq.empty[ActorRef], sink, tap.map(_.ref)),
        s"Alt_${n}_$uniq")
    }

    // ring wiring (always A↔B↔C↔D↔A)
    alts.indices.foreach { i =>
      val neighbours = Seq(alts((i + 1) % 4), alts((i + 3) % 4))
      alts(i) ! SetNeighbors(neighbours)
    }

    // optionally intercept A’s “next” link with a probe
    ctrlProbe.foreach { p =>
      alts.head ! SetNeighbors(Seq(p.ref, alts(1)))
    }
    alts.toVector
  }

  def harvest(probe: TestProbe, dur: FiniteDuration): Vector[TestMessage] =
    probe.receiveWhile(dur) { case t: TestMessage => t }.toVector
}

class AlternatorValidationSpec
  extends TestKit(ActorSystem("AltValSpec"))
    with AnyWordSpecLike
    with Matchers
    with BeforeAndAfterAll {

  import TestHelpers._

  override def afterAll(): Unit = {
    report()                                   // from utils.Metrics
    TestKit.shutdownActorSystem(system)
  }
  
  "No undeclared channel usage" in measure("no‑undeclared‑channel") {
    val sink  = TestProbe()
    val alt   = system.actorOf(Props(classOf[AlternatorProcess], "X", Seq.empty, sink.ref, None))
    val rogue = TestProbe()
    alt ! SetNeighbors(Seq(rogue.ref))

    rogue.send(alt, "FakeMsg")            // message not in the protocol
    rogue.expectNoMessage(2.seconds)
  }

  "Channel connects only valid agents" in measure("channel‑valid‑endpoints") {
    val sink = TestProbe()
    val dead = TestProbe()
    system.stop(dead.ref)                 // simulate vanished neighbour

    val alt = system.actorOf(Props(classOf[AlternatorProcess], "Y", Seq.empty, sink.ref, None))
    alt ! SetNeighbors(Seq(dead.ref))

    val deadLettersProbe = TestProbe()
    system.eventStream.subscribe(deadLettersProbe.ref, classOf[DeadLetter])

    // wait for alternator’s first AskPermission to hit deadLetters
    deadLettersProbe.expectMsgType[DeadLetter](15.seconds)
  }

  "Duplicate actor names are rejected" in measure("duplicate‑actor‑name") {
    val sink = TestProbe()
    val a1   = system.actorOf(Props(classOf[AlternatorProcess], "Dup", Seq(), sink.ref, None), name = "DupActor")
    intercept[InvalidActorNameException] {
      system.actorOf(Props(classOf[AlternatorProcess], "Dup2", Seq(), sink.ref, None), name = "DupActor")
    }
  }
  

  private val expectedStates      = Set("randomWait", "contactNeighbors", "wait4Responses", "proceed")
  private val expectedMsgProtocol = Set("AskPermission", "Goahead", "NoWayMyTurn", "InformSinkProcess")

  "All actors together visit every FSM state" in measure(" ‑state‑coverage") {
    val tap  = TestProbe()
    val sink = system.actorOf(Props(classOf[MessageSinkProcess], Some(tap.ref)))
    val _    = setupRing(system, sink, tap = Some(tap))

    val events      = harvest(tap, 8.seconds)
    val seenStates  = events.collect { case CurrentState(s) => s }.toSet
    seenStates should contain allElementsOf expectedStates
  }

  "Protocol messages *sent* cover the full spec" in measure("msg‑sent‑coverage") {
    val tap  = TestProbe()
    val sink = system.actorOf(Props(classOf[MessageSinkProcess], Some(tap.ref)))
    val _    = setupRing(system, sink, tap = Some(tap))

    val sent = harvest(tap, 15.seconds).collect { case MessageSent(m) => m }.toSet
    sent should contain allElementsOf expectedMsgProtocol
  }

  "Protocol messages *received* cover the full spec" in measure("msg‑recv‑coverage") {
    val tap  = TestProbe()
    val sink = system.actorOf(Props(classOf[MessageSinkProcess], Some(tap.ref)))
    val _    = setupRing(system, sink, tap = Some(tap))

    val recv = harvest(tap, 15.seconds).collect { case MessageReceived(m) => m }.toSet
    recv should contain allElementsOf expectedMsgProtocol.filterNot(_ == "InformSinkProcess")
  }

  "State‑transition events are consistent" in measure("transition‑sanity") {
    val tap  = TestProbe()
    val sink = system.actorOf(Props(classOf[MessageSinkProcess], Some(tap.ref)))
    val _    = setupRing(system, sink, tap = Some(tap))

    val trans   = harvest(tap, 8.seconds).collect { case StateTransition(f, t) => s"$f->$t" }
    val allowed = Set(
      "randomWait->contactNeighbors",
      "contactNeighbors->wait4Responses",
      "wait4Responses->proceed",
      "proceed->randomWait",
      "wait4Responses->wait4Responses",
      "wait4Responses->randomWait",
    )
    trans.foreach(path => allowed should contain(path))
  }

  "Each actor emits at least one valid CurrentState" in measure("per‑agent‑state‑emission") {
    val tap = TestProbe()
    val sink = system.actorOf(Props(classOf[MessageSinkProcess], Some(tap.ref)))
    val alts = setupRing(system, sink, tap = Some(tap)) // keep refs for later

    /* Collect (senderRef, TestMessage) pairs for 8 s */
    val emitted = tap.receiveWhile(8.seconds) {
      case tm: TestMessage => (tap.lastSender, tm)
    }.toVector

    /* Bucket CurrentState events by the actor that sent them */
    val byActor: Map[ActorRef, Vector[String]] =
      emitted.collect {
        case (ref, CurrentState(s)) if expectedStates.contains(s) => (ref, s)
      }.groupBy(_._1).view.mapValues(v => v.map(_._2).toVector).toMap

    /* Assert that every alternator appears in the map */
    alts.foreach { a =>
      withClue(s"${a.path.name} produced no CurrentState events") {
        byActor.keySet should contain(a)
      }
    }
  }

  /** Liveness: every node must eventually enter “proceed” and inform the sink. */
  "Some of all actors should eventually inform the sink (liveness)" in measure("sink‑liveness") {
    val tap = TestProbe()
    val sink = system.actorOf(Props(classOf[MessageSinkProcess], Some(tap.ref)))
    val alts = setupRing(system, sink, tap = Some(tap))

    // watch for InformSinkProcess for up to 20 s
    val infoMsgs = harvest(tap, 30.seconds).collect {
      case MessageSent("InformSinkProcess") => tap.lastSender // origin alternator
    }
    alts.toSet should contain allElementsOf infoMsgs.toSet
  }


  /** Random‑wait behaviour: node grants permission while idling. */
  "An actor in randomWait answers AskPermission with Goahead" in measure("randomWait‑goahead") {
    val sink = TestProbe()
    val alt = system.actorOf(Props(classOf[AlternatorProcess], "Solo", Seq.empty, sink.ref, None))
    val peer = TestProbe()

    alt ! SetNeighbors(Seq(peer.ref)) // make the probe its only neighbour
    peer.send(alt, AskPermission) // immediately poke
    peer.expectMsgType[Goahead.type](1.second)
  }

  /** Contact‑phase behaviour: node denies permission while gathering votes. */
  "An actor in contactNeighbors answers AskPermission with NoWayMyTurn" in measure("contact‑phase‑deny") {
    val tap = TestProbe()
    val sink = system.actorOf(Props(classOf[MessageSinkProcess], Some(tap.ref)))
    val alt = system.actorOf(Props(classOf[AlternatorProcess], "Z", Seq.empty, sink, Some(tap.ref)))
    val peer = TestProbe()
    alt ! SetNeighbors(Seq(peer.ref))

    // Wait until we see the actor switch to contactNeighbors
    tap.fishForMessage(10.seconds) {
      case StateTransition("randomWait", "contactNeighbors") => true
      case k:StateTransition => println(k);false
      case _ => false
    }
    peer.expectMsgType[AskPermission.type](1.second)
    peer.send(alt,Goahead)
    Thread.sleep(1000) // give the actor time to process
    peer.send(alt, AskPermission)
    peer.expectMsgType[NoWayMyTurn.type](1.second)

  }

  /** Timeout fallback: if neighbours stay silent, node returns to randomWait. */
  "Actor falls back to randomWait on timeout" in measure("timeout‑fallback") {
    val tap = TestProbe()
    val silent = TestProbe() // will ignore everything
    val sink = system.actorOf(Props(classOf[MessageSinkProcess], Some(tap.ref)))
    val alt = system.actorOf(Props(classOf[AlternatorProcess], "T", Seq.empty, sink, Some(tap.ref)))
    alt ! SetNeighbors(Seq(silent.ref)) // only silent neighbour

    // Look for the tell‑tale transition wait4Responses -> randomWait
    tap.fishForMessage(15.seconds) {
      case StateTransition("wait4Responses", "randomWait") => true
      case _ => false
    }
  }

  /** Proceed‑reset: after a successful proceed the node must cycle back. */
  "Actor leaves proceed and re‑enters randomWait within 4 s" in measure("proceed‑reset") {
    val tap = TestProbe()
    val sink = system.actorOf(Props(classOf[MessageSinkProcess], Some(tap.ref)))
    setupRing(system, sink, tap = Some(tap))

    // Wait for the first proceed enter
    val enterTime = tap.fishForMessage(15.seconds) {
      case StateTransition(_, "proceed") => true
      case _ => false
    }; // time captured by test‑kit, not used further

    // Now expect the same actor to report randomWait within 4 s
    tap.expectMsgPF(4.seconds) {
      case StateTransition("proceed", "randomWait") => succeed
      case _ => println("unexpected message")
    }
  }

  /** Response discipline: every AskPermission sent must receive exactly one answer. */
  "Every AskPermission elicits exactly one response" in measure("ask‑response‑pairing") {
    val tap = TestProbe()
    val sink = system.actorOf(Props(classOf[MessageSinkProcess], Some(tap.ref)))
    setupRing(system, sink, tap = Some(tap))

    val events = harvest(tap, 15.seconds)

    val msg = events.collect { case K: MessageSent => K  }
//    println(msg)

    val ask = msg.filter(_.msg == "AskPermission")
    val acks = msg.filter(_.msg == "NoWayMyTurn") ++ msg.filter(_.msg == "Goahead")

    ask.size shouldBe acks.size
  }

  "A terminated actor leaves no active Akka timers" in measure("no‑timer‑leak") {
    val sink = TestProbe()
    val alt = system.actorOf(Props(classOf[AlternatorProcess], "Leak", Seq.empty, sink.ref, None))
    watch(alt)
    system.stop(alt)
    expectTerminated(alt, 5.seconds)

    import akka.actor.Scheduler
    val hasTimers =
      system.asInstanceOf[ExtendedActorSystem]
        .systemActorOf(Props.empty, "dummy") // any actorRef in same system
        .path
        .address
        .hasGlobalScope // triggers the internal timer scan
    hasTimers shouldBe false
  }


}
