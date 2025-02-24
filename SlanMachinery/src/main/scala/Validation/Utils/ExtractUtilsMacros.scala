import scala.quoted.*

def getParamNamesImpl[T: Type, R: Type](fExpr: Expr[T => R])(using Quotes): Expr[List[String]] = {
  import quotes.reflect.*
  fExpr.asTerm.underlyingArgument match {
    case Lambda(params, _) =>
      val names = params.map(_.name)
      Expr.ofList(names.map(Expr(_)))
    case other =>
      report.error(s"Expected a lambda expression, but got: ${other.show}")
      '{ Nil }
  }
}
