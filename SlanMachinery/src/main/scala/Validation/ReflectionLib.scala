package Validation

import GenericDefinitions.{Ctx, ProcessingContext}

import scala.quoted.*

object ReflectionLib {

//  val logger: Logger = CreateLogger(classOf[String])

  object ASTPrinter {

    // Note the return type is now String
    inline def printAST[T](inline expr: T): String = ${ printASTImpl('expr) }

    // Return Expr[String] here, not Expr[Unit]
    private def printASTImpl(expr: Expr[Any])(using Quotes): Expr[String] = {
      import quotes.reflect.*

      val tree      = expr.asTerm
      val prettyTree = tree.show(using Printer.TreeStructure) // Pretty-printed AST

      // Return the pretty-printed tree as an Expr[String]
      Expr(prettyTree)
    }
  }

  object extractSource {
    inline def sourceCode[T](inline expr: T): String = ${ sourceCodeImpl('expr) }

    private def sourceCodeImpl(expr: Expr[Any])(using Quotes): Expr[String] = {
      import quotes.reflect.*

      val tree = expr.asTerm
      val sourceCode = tree.show
      Expr(sourceCode)
    }
  }

  object doesInspector:

    inline def inspectDoesBlock[T](inline expr: T)
    : (ProcessingContext => Unit, ProcessingContext => Any, String, String) =
      ${ inspectDoesImpl('expr) }

    private def inspectDoesImpl(expr: Expr[Any])
                               (using Quotes)
    : Expr[(ProcessingContext => Unit,
      ProcessingContext => Any,
      String, String)] =
      import quotes.reflect.*

      expr.asTerm match
        case Inlined(_, _, Inlined(_, _, Block(stats, last))) =>
          val stmtsBlock = Block(stats, Literal(UnitConstant()))
          val exprBlock  = Block(Nil, last)

          '{
            (
              (pc: ProcessingContext) =>                         // <- run‑time arg
                ProcessingContext.withCtx(pc) {
                  given ProcessingContext = pc
                  ${ stmtsBlock.asExprOf[Unit] }                 // user stmts
                },

              /* main body: evaluate final expression / PF in same way */
              (pc: ProcessingContext) =>
                ProcessingContext.withCtx(pc) {
                  given ProcessingContext = pc
                  ${ exprBlock.asExprOf[Any] }                   // user expr
                },

              ${ Expr(stmtsBlock.show) },
              ${ Expr(exprBlock.show) }
            )
          }

        case _ =>
          quotes.reflect.report.error("Block expression cannot be processed")
          '{ throw new Exception("Block expression cannot be processed") }

  object IdentInspector {
    inline def inspect[T](inline expr: T): String = ${ inspectImpl('expr) }

    private def inspectImpl(expr: Expr[Any])(using Quotes): Expr[String] = {
      import quotes.reflect.*

      val term = expr.asTerm // Convert expression to AST node
      val sb = new StringBuilder()

      sb.append("Inspecting expression:\n")
      sb.append(s"${term}\n\n")

      def processTree(tree: Tree): Unit = tree match {

        case Inlined(_, _, expr) =>
          processTree(expr)
        case Ident(name) =>
          sb.append(s"Identifier: $name\n")

          // Get the symbol to check what it refers to
          val sym = tree.symbol
          sb.append(s"Symbol info: ${sym.tree.show}\n")
          sb.append(s"Symbol tree: ${sym.tree}\n")
          processTree(sym.tree)

        case Block(statements, expr) =>
          sb.append("Block found:\n" + statements.map(_.show).mkString("\n"))
          sb.append("\n Block Expression:\n" + expr + '\n' + expr.show)
          statements.foreach(processTree)
          processTree(expr)

        case DefDef(name, _, _, rhs) =>
          sb.append(s"DefDef: $name\n")
          sb.append("RHS:\n")
          sb.append(s"{$rhs}")
          rhs.foreach(processTree)

        case ValDef(name, tpt, rhs) =>
          sb.append(s"ValDef: $name\n")
          sb.append(s"Type: ${tpt.show}\n")
          sb.append("RHS:\n")
          sb.append(s"{$rhs}")
          rhs.foreach(processTree)

        case Apply(fun, args) =>
          sb.append("Function application:\n")
          processTree(fun)
          args.foreach(processTree)

        case Select(qualifier, name) =>
          processTree(qualifier)

          sb.append(s"called process Selection: $name on ${qualifier.show}\n")

        case other =>
          sb.append(s"Other tree node: ${other}\n")
      }

      processTree(term)
      Expr(sb.toString()) // Return as an expression
    }

  }
}

