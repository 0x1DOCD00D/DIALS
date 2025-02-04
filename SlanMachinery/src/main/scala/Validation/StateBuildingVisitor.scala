package Validation

import GenericDefinitions._
import Utilz.{Constants, CreateLogger}
import cats.implicits._
import org.slf4j.Logger

class StateBuildingVisitor(val state: ValidationState) extends Visitor[ValidationState] {

  val logger: Logger = CreateLogger(classOf[StateBuildingVisitor])

  def apply(model: ModelEntity): ValidationState = visit(model)

  override def visit(model: ModelEntity): ValidationState = {


    // Check if the model has already been visited
    if (state.visitedEntities.contains(model.hashCode().toString)) {
      logger.info(s"Visiting model: ${model.name}")
      state
    } else {
      // Create an updated state with the current model marked as visited
      val updatedState = state |+| ValidationState.empty.copy(
        visitedEntities = state.visitedEntities + model.hashCode().toString
      )

      // Process each connection functionally, passing the updated state along
      model.connections.foldLeft(updatedState) { (accState, conn) =>
        processConnection(conn, accState)
      }
    }
  }

  private def processConnection(c: Connection, currentState: ValidationState): ValidationState = c match {
    case CompletedChain(from, to, channel, _) =>
      val fromState = from._1.accept(StateBuildingVisitor(currentState))
      val toState = to._1.accept(StateBuildingVisitor(fromState))
      val channelState = channel.accept(StateBuildingVisitor(toState))
      channelState

    case _ =>
      logger.error(s"Not a completed chain")
      currentState
  }

  def visit(alias: EntityInstanceAlias): ValidationState = {


    val aliasHash = alias.hashCode().toString

    if (state.visitedEntities.contains(aliasHash)) {
      state
    } else {
      logger.info(s"Visiting alias: ${alias.alias}")
      val updatedState = state |+| ValidationState.empty.copy(
        visitedEntities = state.visitedEntities + aliasHash,
        aliasNameToHashes = state.aliasNameToHashes.updated(
          alias.ent.get.asInstanceOf[AgentEntity].name,
          state.aliasNameToHashes.getOrElse(alias.ent.get.asInstanceOf[AgentEntity].name, Map.empty) +
            (alias.alias -> (state.aliasNameToHashes
              .getOrElse(alias.ent.get.asInstanceOf[AgentEntity].name, Map.empty)
              .getOrElse(alias.alias, Set.empty) + aliasHash))
        )
      )

      alias.ent.get.accept(StateBuildingVisitor(updatedState)).asInstanceOf[ValidationState]
    }
  }

  override def visit(agent: AgentEntity): ValidationState = {


    val agentHash = agent.hashCode().toString

    if (state.visitedEntities.contains(agentHash)) {
      state
    } else {
      logger.info(s"Visiting agent: ${agent.name}")
      state |+| ValidationState.empty.copy(
        visitedEntities = state.visitedEntities + agentHash,
        agentNameToHashes = state.agentNameToHashes.updated(
          agent.name, state.agentNameToHashes.getOrElse(agent.name, Set.empty) + agentHash
        )
      )
    }
  }

  override def visit(channel: ChannelEntity): ValidationState = {


    val channelHash = channel.hashCode().toString

    if (state.visitedEntities.contains(channelHash)) {
      state
    } else {
      logger.info(s"Visiting channel: ${channel.name}")
      state |+| ValidationState.empty.copy(
        visitedEntities = state.visitedEntities + channelHash,
        channelNameToHashes = state.channelNameToHashes.updated(
          channel.name, state.channelNameToHashes.getOrElse(channel.name, Set.empty) + channelHash
        )
      )
    }
  }

  override def visit(group: GroupEntity): ValidationState = {


    val groupHash = group.hashCode().toString

    if (state.visitedEntities.contains(groupHash)) {
      state
    } else {
      logger.info(s"Visiting group")
      state |+| ValidationState.empty.copy(
        visitedEntities = state.visitedEntities + groupHash
      )
    }
  }

  override def visit(entity: DialsEntity): ValidationState = entity match {
    case agent: AgentEntity         => visit(agent)
    case model: ModelEntity         => visit(model)
    case channel: ChannelEntity     => visit(channel)
    case alias: EntityInstanceAlias => visit(alias)
    case group: GroupEntity         => visit(group)
    case _ =>
      logger.error(s"Unknown DialsEntity type: $entity")
      state
  }
}
