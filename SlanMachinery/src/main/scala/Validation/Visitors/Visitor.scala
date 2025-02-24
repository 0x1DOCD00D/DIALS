package Validation.Visitors

import GenericDefinitions.{AgentEntity, GroupEntity, ModelEntity, ChannelEntity, DialsEntity}

trait VisitorState

// Single visit function for dials entity and pattern matching to specific entity and call function

trait Visitor[S <: VisitorState] {
  def visit(model: ModelEntity): S
  def visit(agent: AgentEntity): S
  def visit(channel: ChannelEntity): S
  def visit(group: GroupEntity): S
  def visit(genericDialsEntity: DialsEntity): S
}