package GenericDefinitions

import akka.actor.ActorRef
import scala.collection.mutable

import akka.actor.ActorRef
import scala.collection.mutable


import akka.actor.ActorRef
import scala.util.DynamicVariable

final case class ProcessingContext(
                                    var selfRef: ActorRef,
                                    var actorKind: String = "",
                                    var resources: Map[String, ActorRef] = Map.empty,
                                    var channels: Map[String, ActorRef] = Map.empty,
                                    var executionFlag: Boolean = false
                                  ){
    override def toString: String =
        s"ProcessingContext(selfRef=$selfRef, actorKind=$actorKind, executionFlag=$executionFlag)" +
            s"\nresources=${resources.keys.mkString(",")}" +
            s"\nchannels=${channels.keys.mkString(",")}"
}

object ProcessingContext:
  given defaultCtx : ProcessingContext = current
  private val local = new DynamicVariable[ProcessingContext](ProcessingContext(ActorRef.noSender))
  def withCtx[T](ctx: ProcessingContext)(body: => T): T = local.withValue(ctx)(body)
  def current: ProcessingContext = local.value


// sugar helpers --------------------------------------------------------------
object Ctx {
  def current : ProcessingContext = ProcessingContext.current
  
  def self: ActorRef = ProcessingContext.current.selfRef

  def kind: String = ProcessingContext.current.actorKind

  def flag: Boolean = ProcessingContext.current.executionFlag

  def resources: Map[String, ActorRef] = ProcessingContext.current.resources

  def channels: Map[String, ActorRef] = ProcessingContext.current.channels

  def setFlag(b: Boolean): Unit = ProcessingContext.current.executionFlag = b

  def setKind(k: String): Unit = ProcessingContext.current.actorKind = k

  def setResources(m: Map[String, ActorRef]): Unit = ProcessingContext.current.resources = m

  def setChannels(m: Map[String, ActorRef]): Unit = ProcessingContext.current.channels = m

  def toStringFull: String = ProcessingContext.current.toString
}
