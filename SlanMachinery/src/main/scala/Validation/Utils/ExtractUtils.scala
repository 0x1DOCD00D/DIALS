package Validation.Utils

import GenericDefinitions.{AgentEntity, BehaviorEntity, ChannelEntity, DialsEntity, EntityInstanceAlias, GenericMessageTemplate, GroupEntity, MessageEntity, PatternMatch4Messages, ProcessingContext}

import scala.collection.mutable.ListBuffer
import scala.runtime.AbstractPartialFunction
import scala.quoted.*
import Utilz.CreateLogger

object ExtractUtils {

  val logger = CreateLogger(classOf[String])

  def extractName(entity: DialsEntity): String = entity match {
    case agent: AgentEntity             => agent.name
    case group: GroupEntity             => group.getName
    case alias: EntityInstanceAlias     => alias.ent.getOrElse(None).asInstanceOf[AgentEntity].name
    case _                              => throw new IllegalArgumentException("Unknown DialsEntity type")
  }

  def extractMessages(entity: ChannelEntity): Set[MessageEntity] =
    entity.messages.map(_._1).toSet

  def extractBehaviourMessage(using ctx: ProcessingContext)(entity: BehaviorEntity, messages: Set[MessageEntity]): Set[String] = {
    val triggerSet: Set[String] = entity.triggerMsgs.iterator.map(_.name).toSet
    val actualActionSet: Set[String] = entity.actualActions.iterator.flatMap(pfWithCtx=>extractMessageNamesFromPF(pfWithCtx(ctx),messages)).toSet
    logger.info(s"Behavior ${entity.name} has triggers: ${triggerSet.mkString(", ")} and actions: ${actualActionSet.mkString(", ")}")
    triggerSet ++ actualActionSet
  }

  def extractMessageNamesFromPF(pf: PartialFunction[Any, Unit], messages: Set[MessageEntity]): Set[String] = {
    val messageNames = ListBuffer.empty[String]

    messages.foreach { msg =>
      if (pf.isDefinedAt(GenericMessageTemplate(msg.name,List.empty,None))) {
//        pf(msg)
//        check if its defined for anything other than the message name
        if (pf.isDefinedAt(Int.MaxValue)){
            logger.error(s"Partial function $pf is defined for any value, cant check for pattern match with generic message")
        }else{
          messageNames += msg.name
        }

      }
      else{
//        pf(GenericMessageTemplate(msg.name,List.empty,None))
//        pf(msg)
        logger.error(s"Message $msg is not handled by the behavior")
      }
    }
    messageNames.toSet
  }

}
