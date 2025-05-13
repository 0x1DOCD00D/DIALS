import GenericDefinitions.ProcessingContext

object CtxOverride {
  private val dynamicCtx = new scala.util.DynamicVariable[Option[ProcessingContext]](None)

  inline given ctx: ProcessingContext = dynamicCtx.value.getOrElse {
    throw new IllegalStateException("No ProcessingContext bound")
  }

  def withCtx[T](ctx: ProcessingContext)(f: => T): T =
    dynamicCtx.withValue(Some(ctx))(f)
}