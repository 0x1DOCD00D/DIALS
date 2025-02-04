package Validation

import GenericDefinitions.{AgentEntity, GroupEntity, ModelEntity, ChannelEntity, DialsEntity}

trait VisitorState


trait Visitor[S <: VisitorState] {
  def visit(model: ModelEntity): S
  def visit(agent: AgentEntity): S
  def visit(channel: ChannelEntity): S
  def visit(group: GroupEntity): S
  def visit(genericDialsEntity: DialsEntity): S
}