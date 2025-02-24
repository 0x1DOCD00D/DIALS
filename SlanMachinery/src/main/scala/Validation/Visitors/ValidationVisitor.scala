package Validation.Visitors

import GenericDefinitions.*
import Utilz.{Constants, CreateLogger}
import Validation.EntityValidation.Agent.AgentValidations
import Validation.EntityValidation.Channel.ChannelValidations
import org.slf4j.Logger
import cats.implicits._
import Validation.States.ValidationState
import Validation.Results.ValidationResult

class ValidationVisitor(val state: ValidationState, val result: ValidationResult) extends Visitor[ValidationResult] {

  val logger: Logger = CreateLogger(classOf[ValidationVisitor])

  def apply(model: ModelEntity): ValidationResult = {
    model.accept(ValidationVisitor(state, result))
  }

  override def visit(model: ModelEntity): ValidationResult = {
    val modelHash = model.hashCode().toString

    if (result.visitedEntities.contains(modelHash)) return result

    logger.info(s"Visiting model: ${model.name}")

    val updatedRes = result.copy(visitedEntities = result.visitedEntities :+ modelHash)

    model.connections.foldLeft(updatedRes) { (accRes, conn) =>
      processConnection(conn, accRes)
    }
  }

  private def processConnection(c: Connection, accRes: ValidationResult): ValidationResult = c match {
    case CompletedChain(from, to, channel, _) =>
      val fromState = from._1.accept(ValidationVisitor(state, accRes))
      val toState = to._1.accept(ValidationVisitor(state, fromState))
      val channelState = channel.accept(ValidationVisitor(state, toState))
      channelState

    case _ =>
      logger.error(s"Not a completed chain")
      accRes
  }

  def visit(alias: EntityInstanceAlias): ValidationResult = {
    val aliasHash = alias.hashCode().toString

    if (result.visitedEntities.contains(aliasHash)) return result

    val updatedRes = result.copy(visitedEntities = result.visitedEntities :+ aliasHash)
    logger.info(s"Visiting alias: ${alias.alias}")

    alias.ent.get.accept(ValidationVisitor(state, updatedRes))
  }

  override def visit(agent: AgentEntity): ValidationResult = {
    val agentHash = agent.hashCode().toString

    if (result.visitedEntities.contains(agentHash)) return result

    val updatedRes = result.copy(visitedEntities = result.visitedEntities :+ agentHash)
    logger.info(s"Visiting agent: ${agent.name}")

    val agentResult = AgentValidations.validate(agent, state)
    ValidationVisitor(state, updatedRes |+| agentResult).result
  }

  override def visit(channel: ChannelEntity): ValidationResult = {
    val channelHash = channel.hashCode().toString

    if (result.visitedEntities.contains(channelHash)) return result

    val updatedRes = result.copy(visitedEntities = result.visitedEntities :+ channelHash)
    logger.info(s"Visiting channel: ${channel.name}")

    val channelResult = ChannelValidations.validate(channel, state)
    ValidationVisitor(state, updatedRes |+| channelResult).result
  }

  override def visit(group: GroupEntity): ValidationResult = {
    val groupHash = group.hashCode().toString

    if (result.visitedEntities.contains(groupHash)) return result

    val updatedRes = result.copy(visitedEntities = result.visitedEntities :+ groupHash)

    logger.info(s"Visiting group")

    ValidationVisitor(state, updatedRes).result
  }

  override def visit(entity: DialsEntity): ValidationResult = entity match {
    case agent: AgentEntity       => visit(agent)
    case model: ModelEntity       => visit(model)
    case channel: ChannelEntity   => visit(channel)
    case alias: EntityInstanceAlias => visit(alias)
    case group: GroupEntity       => visit(group)
    case _ =>
      logger.error(s"Unknown DialsEntity type: $entity")
      result
  }
}
