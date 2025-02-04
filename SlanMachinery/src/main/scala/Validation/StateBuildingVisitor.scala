package Validation

import GenericDefinitions.{AgentEntity, ChannelEntity, CompletedChain, Connection, DialsEntity, EntityInstanceAlias, GroupEntity, ModelEntity}
import Utilz.{Constants, CreateLogger}
import cats.syntax.semigroup.*
import cats.implicits._
import org.slf4j.Logger
import Validation.ValidationState.validationStateMonoid

class StateBuildingVisitor(val state: ValidationState) extends Visitor[ValidationState] {

  val logger: Logger = CreateLogger(classOf[StateBuildingVisitor])

  def apply(model: ModelEntity): ValidationState = visit(model)

  override def visit(model: ModelEntity): ValidationState = {
    logger.info(s"Visiting model: ${model.name}")

    // Create an immutable updated state
    val baseState = state |+| ValidationState.empty.copy(
      visitedEntities = state.visitedEntities + model.hashCode().toString
    )

    // Process each connection functionally
    model.connections.foldLeft(baseState) { (accState, conn) =>
      accState |+| processConnection(conn)
    }
  }

  private def processConnection(c: Connection): ValidationState = c match {
    case CompletedChain(from, to, channel, _) =>
      val fromState = from._1.accept(this)
      val toState = to._1.accept(this)
      val channelState = channel.accept(this)
      fromState |+| toState |+| channelState

    case _ =>
      logger.error(s"Not a completed chain")
      ValidationState.empty
  }

   def visit(alias: EntityInstanceAlias): ValidationState = {
    logger.info(s"Visiting alias: ${alias.alias}")

    val updatedState = state |+| ValidationState.empty.copy(
      visitedEntities = state.visitedEntities + alias.hashCode().toString,
      aliasNameToHashes = state.aliasNameToHashes.updated(
        alias.ent.get.asInstanceOf[AgentEntity].name,
        state.aliasNameToHashes.getOrElse(alias.ent.get.asInstanceOf[AgentEntity].name, Map.empty) +
          (alias.alias -> (state.aliasNameToHashes
            .getOrElse(alias.ent.get.asInstanceOf[AgentEntity].name, Map.empty)
            .getOrElse(alias.alias, Set.empty) + alias.hashCode().toString))
      )
    )

    updatedState |+| alias.ent.get.accept(this).asInstanceOf[ValidationState]
  }

  override def visit(agent: AgentEntity): ValidationState = {
    logger.info(s"Visiting agent: ${agent.name}")

    state |+| ValidationState.empty.copy(
      visitedEntities = state.visitedEntities + agent.hashCode().toString,
      agentNameToHashes = state.agentNameToHashes.updated(
        agent.name, state.agentNameToHashes.getOrElse(agent.name, Set.empty) + agent.hashCode().toString
      )
    )
  }

  override def visit(channel: ChannelEntity): ValidationState = {
    logger.info(s"Visiting channel: ${channel.name}")

    state |+| ValidationState.empty.copy(
      visitedEntities = state.visitedEntities + channel.hashCode().toString,
      channelNameToHashes = state.channelNameToHashes.updated(
        channel.name, state.channelNameToHashes.getOrElse(channel.name, Set.empty) + channel.hashCode().toString
      )
    )
  }

  override def visit(group: GroupEntity): ValidationState = {
    logger.info(s"Visiting group")
    state
  }

  override def visit(entity: DialsEntity): ValidationState = entity match {
    case agent: AgentEntity        => visit(agent)
    case model: ModelEntity        => visit(model)
    case channel: ChannelEntity    => visit(channel)
    case alias: EntityInstanceAlias => visit(alias)
    case group: GroupEntity        => visit(group)
    case _ =>
      logger.error(s"Unknown DialsEntity type: $entity")
      state
  }
}
