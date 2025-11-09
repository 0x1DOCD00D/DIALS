# DIALS

DIstributed Agent Language for Simulations. A Scala EDSL for writing agent based simulations that compiles to Akka actors. It keeps your model small, your runtime fast, and your reasoning sane.

## Why this exists

Plain Akka gives you power, and a combinatorial explosion of states. DIALS pins behavior to explicit states and channels, so the global state space grows slower and is easier to verify. The paper shows the idea, the π calculus backbone, and the measured wins. See the diagram of concepts on page 3 and the reduction bounds discussion in sections 3.2 and 3.3. 

## Project layout

GenericSimUtilities
Reusable bits for simulations, probability helpers, message and resource utilities. The repo exposes this as a top level directory.

SimulationEngine
Akka runtime scaffolding, message envelopes, and the code generation glue that turns the DSL IR into running actors.

SlanMachinery
The DSL core. Dynamic keywords, state machine building blocks, and the IR plus validation rules.

project
SBT build definitions.

src/main/scala
Entry points and examples.

build.sbt and lightbend.sbt
Build configuration.
These directories and files are visible at the repository root. ([GitHub][1])

## Install and build

Clone the repo, then run sbt.

```
git clone https://github.com/0x1DOCD00D/DIALS
cd DIALS
sbt compile
sbt test
```

To run an example, use one of the mains under src/main/scala.

```
sbt "runMain dials.examples.AlternatorMain"
```

## DIALS in one page

The DSL models agents as finite state machines, with messages, channels, and resources. You write a computation graph. The engine generates Akka actors and binds the handlers for you. The paper’s examples are referenced with page numbers below. 

Agents and states
Figure 4 on page 5 shows a typical agent with three states and transitions. The same style is used here.

```scala
(agent PingAgent) has {
  (state Idle) behaves {
    (action Wait) does {
      case _ =>
        (dispatch Ping) send (channel Net)
    }
  } switch2 (state Waiting)

  (state Waiting) behaves {
    (action Respond) does {
      case (received Pong) =>
        println("Received Pong")
    }
  }
} autotrigger (state Idle)
```

Messages and behaviors
The message abstraction and the onEventRule helper are shown in Figures 5 and 6 on page 5. The idea is simple, match the received name, unpack values and fields, do the work.

```scala
(dispatch mx) comprises {
  (field f1) := (10, 3.14)
  (field f2) := (pdf Uniform) as (0, 1)
}

(action ReceiveResponse) does {
  onEventRule {
    (received RcvdMessage) -> { (v, f) =>
      respond (dispatch mx)
    }
  }
}
```

Channels
Channels connect agents and can transform messages in flight. See Figure 7 on page 5.

```scala
(channel Net) transports {
  (dispatch Ping);
  (dispatch Pong)
}
```

Resources
Resources are actor backed storage, so reads and writes are serialized. See Figure 8 on page 6.

```scala
(resource Storage) contains {
  (resource tpl) := (10, 20, 3.14)
}
```

Groups
You can group agents to share channels and resources. Figure 9 on page 6 shows a region and zones.

```scala
(agent DC1) joins (group A) of (group West)

(group West) comprises {
  (group A)
  (group B) comprises {
    (resource Storage) := 10
    (agent DC2)
    (agent DC3)
  }
}
```

Topology and model
The alternator case study shows how to declare agent counts and link them in a ring to a sink. See Figure 13 on page 8 for the full spec.

```scala
(model distributedAlternatorSimulator) `is defined as` {
  |(agent AlternatorProcess)| := exactly (instance A)
  |(agent AlternatorProcess)| := exactly (instance B)
  |(agent AlternatorProcess)| := exactly (instance C)
  |(agent AlternatorProcess)| := exactly (instance D)
  |(agent MessageSinkProcess)| := exactly (instance S)
} `is defined as` {
  (agent A) < > (channel ControlAction) < > (agent B) < >
  (channel ControlAction) < > (agent C) < >
  (channel ControlAction) < > (agent D) < >
  (channel ControlAction) < > (agent A)

  (agent A) > (channel Data) ~> (agent S)
  (agent B) > (channel Data) ~> (agent S)
  (agent C) > (channel Data) ~> (agent S)
  (agent D) > (channel Data) ~> (agent S)
}
```

## How the DSL becomes Akka

The translation is mechanical.

1. Each agent becomes one Akka actor.
2. Each state becomes a Receive block, bound by context.become when you switch.
3. Sends on channels become actor selections or typed tell to the channel endpoint.
4. Resources are actors with message based gets and sets.
5. The model graph wires actor paths.

Figure 10 on page 7 shows a PingAgent to PingActor mapping.

DIALS code

```scala
(agent PingAgent) has {
  (state Idle) behaves { (action Wait) does { case _ =>
    (dispatch Ping) send (channel Net)
  } } switch2 (state Waiting)
  (state Waiting) behaves { (action Respond) does {
    case (received Pong) => println("Received Pong")
  } }
} autotrigger (state Idle)
```

Generated Akka sketch

```scala
import akka.actor._

final case class GenericMessage(name: String, values: List[Double] = Nil, fields: Option[List[Any]] = None)

final class PingActor extends Actor {
  def idle: Receive = {
    case "Wait" =>
      context.actorSelection("/user/Net") ! GenericMessage("Ping")
      context.become(waiting)
  }

  def waiting: Receive = {
    case GenericMessage("Pong", _, _) =>
      println("Received Pong")
  }

  def receive: Receive = idle
}
```

Why this works
The DSL binds which messages a state can handle and what transitions exist, so the generator can emit one partial function per state and deterministic switches between them. This keeps the runtime faithful to the model and avoids free form handlers sprinkled across a big actor. See the discussion around Figure 10. 

## Model checking in practice

The validator walks the IR of agents, states, channels, and resources, then enforces rules like no undeclared channel usage, all states reachable, and no dead end transitions. The type class based validator and a sample rule are shown in Figure 11 on page 8.

```scala
check("AgentHasStates") always {
  forAll(agents) { agent => agent.states.nonEmpty }
}
```

The alternator study found more logic bugs faster and with less memory than an Akka TestKit suite. The table on page 9 summarizes detections and timing. Use the checker for structural soundness; keep a few runtime tests for timing and timeout behavior. 

## Quick cookbook

Define messages

```scala
(dispatch AskPermission)
(dispatch GoAhead)
(dispatch InformSinkProcess) := (pdf Normal) as (100, 10)
```

Define an agent with a timer and a guard

```scala
(agent AlternatorProcess) has {
  (resource responseCount) := 0
  (resource numberOfNeighbors) := 2

  (state RandomWait) behaves { /* timer fires */ }
    switchOnTimeout (state ContactNeighbors) timeout 3.seconds

  (state ContactNeighbors) onSwitch {
    (dispatch AskPermission) send (channel ControlAction)
  } switch2 (state WaitForResponses)

  (state WaitForResponses) behaves {
    (action Receive) does {
      onEventRule {
        (received GoAhead) -> { (v,f) =>
          (resource responseCount) := (resource responseCount).getValues.head.toInt + 1
        }
      }
    }
  } switch2 (state Proceed) when {
    (resource responseCount).getValues.head.toInt == (resource numberOfNeighbors).getValues.head.toInt
  }

  (state Proceed) onSwitch {
    (dispatch InformSinkProcess) send (channel Data)
  } switch2 (state RandomWait)
}
```

Wire the topology

```scala
(model distributedAlternator) `is defined as` {
  |(agent AlternatorProcess)| := exactly (instance A)
  |(agent AlternatorProcess)| := exactly (instance B)
  |(agent AlternatorProcess)| := exactly (instance C)
  |(agent AlternatorProcess)| := exactly (instance D)
  |(agent MessageSinkProcess)| := exactly (instance S)
} `is defined as` {
  (agent A) < > (channel ControlAction) < > (agent B) < >
  (channel ControlAction) < > (agent C) < >
  (channel ControlAction) < > (agent D) < >
  (channel ControlAction) < > (agent A)

  (agent A) ~> (channel Data) ~> (agent S)
  (agent B) ~> (channel Data) ~> (agent S)
  (agent C) ~> (channel Data) ~> (agent S)
  (agent D) ~> (channel Data) ~> (agent S)
}
```

## Contributing

Open an issue, propose a rule or a feature, keep the state space small, and the code clear.

## License
 ([GitHub][1])
