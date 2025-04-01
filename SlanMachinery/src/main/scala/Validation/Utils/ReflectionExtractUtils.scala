package Validation.Utils
import scala.reflect.runtime.universe
import scala.reflect.runtime.universe._

object ReflectionExtractUtils:

  import scala.tools.reflect.ToolBox

  val tb: ToolBox[universe.type] = universe
    .runtimeMirror(getClass.getClassLoader)
    .mkToolBox()

  def extractDynamicSelects(tree: Tree): List[(List[String], String)] = {

    def collectNames(t: Tree): List[String] = t match {
      case Select(inner, TermName(name)) => collectNames(inner) :+ name
      case Ident(TermName(name))         => List(name)
      case _                             => Nil
    }

    def matchesDynamicAccess(t: Tree): Option[(List[String], String)] = t match {
      case Apply(Select(base, TermName("selectDynamic")), List(Literal(Constant(name: String)))) =>
        Some((collectNames(base), name))
      case _ =>
        None
    }

    def loop(t: Tree): List[(List[String], String)] = {
      val here = matchesDynamicAccess(t).toList
      val children = t.children.flatMap(loop)
      here ++ children
    }

    loop(tree)
  }

  def checkResourceAccess(source: String): Set[String] = {
    val tree = tb.parse(source)
    val results = extractDynamicSelects(tree)
    val resourceAccesses = results.filter(_._1.contains("resource"))
    resourceAccesses.map { case (path, name) => name }.toSet
  }


  def main(args: Array[String]): Unit = {
    val code =
      """GenericDefinitions.PatternMatch4Messages.onEventRule(scala.Predef.ArrowAssoc[java.lang.String](GenericDefinitions.Keywords.received.selectDynamic("AskPermission")).->[scala.Function2[scala.collection.immutable.List[scala.Double], scala.Option[scala.collection.immutable.List[GenericDefinitions.GenericMessageTemplate]], scala.Unit] {
        |  def apply(values: scala.collection.immutable.List[scala.Double], fields: scala.Option[scala.collection.immutable.List[GenericDefinitions.GenericMessageTemplate]]): scala.Unit
        |}](((v: scala.collection.immutable.List[scala.Double], f: scala.Option[scala.collection.immutable.List[GenericDefinitions.GenericMessageTemplate]]) => {
        |  GenericDefinitions.Keywords.resource.selectDynamic("Storage").:=[scala.AnyVal | GenericDefinitions.DialsEntity](v.asInstanceOf[scala.AnyVal | GenericDefinitions.DialsEntity])(GenericDefinitions.InputDataProcessor.given_TypeInfo_T[scala.AnyVal | GenericDefinitions.DialsEntity](scala.reflect.ClassTag.apply[scala.AnyVal | GenericDefinitions.DialsEntity](classOf[java.lang.Object])))
        |  GenericDefinitions.Keywords.dispatch.selectDynamic("NoWayMyTurn").respond(GenericDefinitions.SenderAgent)
        |  ()
        |}))).orElse[scala.Any, scala.Unit](GenericDefinitions.PatternMatch4Messages.onEventRule(scala.Predef.ArrowAssoc[java.lang.String](GenericDefinitions.Keywords.received.selectDynamic("Goahead")).->[scala.Function2[scala.collection.immutable.List[scala.Double], scala.Option[scala.collection.immutable.List[GenericDefinitions.GenericMessageTemplate]], scala.Unit] {
        |  def apply(values: scala.collection.immutable.List[scala.Double], fields: scala.Option[scala.collection.immutable.List[GenericDefinitions.GenericMessageTemplate]]): scala.Unit
        |}](((`v₂`: scala.collection.immutable.List[scala.Double], `f₂`: scala.Option[scala.collection.immutable.List[GenericDefinitions.GenericMessageTemplate]]) => GenericDefinitions.Keywords.resource.selectDynamic("responseCount").:=[scala.Int](GenericDefinitions.Keywords.resource.selectDynamic("responseCount").getValues.toList.take(1).head.toInt.+(1))(GenericDefinitions.InputDataProcessor.given_TypeInfo_T[scala.Int](scala.reflect.ClassTag.apply[scala.Int](classOf[scala.Int]))))))).orElse[scala.Any, scala.Unit](GenericDefinitions.PatternMatch4Messages.onEventRule(scala.Predef.ArrowAssoc[java.lang.String](GenericDefinitions.Keywords.received.selectDynamic("NoWayMyTurn")).->[scala.Function2[scala.collection.immutable.List[scala.Double], scala.Option[scala.collection.immutable.List[GenericDefinitions.GenericMessageTemplate]], scala.Unit] {
        |  def apply(values: scala.collection.immutable.List[scala.Double], fields: scala.Option[scala.collection.immutable.List[GenericDefinitions.GenericMessageTemplate]]): scala.Unit
        |}](((`v₃`: scala.collection.immutable.List[scala.Double], `f₃`: scala.Option[scala.collection.immutable.List[GenericDefinitions.GenericMessageTemplate]]) => {
        |  val agentID: scala.Int = `v₃`.asInstanceOf[scala.List[scala.Double]].head.toInt
        |  if (GenericDefinitions.Keywords.resource.selectDynamic("ProcessID").getValues.toList.head.toInt.>(agentID)) GenericDefinitions.Keywords.resource.selectDynamic("responseCount").:=[scala.Int](GenericDefinitions.Keywords.resource.selectDynamic("responseCount").getValues.toList.head.toInt.+(1))(GenericDefinitions.InputDataProcessor.given_TypeInfo_T[scala.Int](scala.reflect.ClassTag.apply[scala.Int](classOf[scala.Int]))) else ()
        |}))))""".stripMargin

    val tree = tb.parse(code)

    println("Raw AST:")
    println(tree)

    println("\nExtracted access path:")
    val results = extractDynamicSelects(tree)
    results.foreach(println)
    println("\nPretty-printed AST:")
    println(universe.showCode(tree))

    println("\nFull tree structure:")
    println(universe.showRaw(tree, printTypes = true))

    println(checkResourceAccess(code))
  }
