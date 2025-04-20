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

  def checkChannelAccess(source: String): Set[String] = {
    val tree = tb.parse(source)
    val results = extractDynamicSelects(tree)
    val channelAccesses = results.filter(_._1.contains("channel"))
    channelAccesses.map { case (path, name) => name }.toSet
  }

  def checkMessageAccess(source: String): Set[String] = {
    val tree = tb.parse(source)
    val results = extractDynamicSelects(tree)
    val messageAccesses = results.filter(_._1.contains("dispatch"))
    messageAccesses.map { case (path, name) => name }.toSet
  }

  /** (messageName , channelName) for every   msg.send(channel)   we spot,
   * even if the message was first bound to a val and sent later.
   */
  def extractSendChannelPairs(source: String): Set[(String, String)] = {
    val tree = tb.parse(source)

    /* ------------------------------------------------------------------
     * 1)  Collect every   val x = <rhs>   into a simple “symbol table”.
     *     For our use‑case a flat map is enough (no shadowing in examples).
     * ------------------------------------------------------------------ */
    def collectDefs(t: Tree): Map[String, Tree] = t match {
      case ValDef(_, TermName(name), _, rhs) =>
        Map(name -> rhs) ++ t.children.flatMap(collectDefs)
      case _ =>
        t.children.flatMap(collectDefs).toMap
    }

    val defs: Map[String, Tree] = collectDefs(tree)

    /* ------------------------------------------------------------------
     * 2)  Given any expression, try to find the first dynamic‑select that
     *     belongs to the requested ‘kind’ ("dispatch" | "channel").
     *     – If the expression is an Ident, look it up in defs and search
     *       inside its RHS recursively.
     * ------------------------------------------------------------------ */
    def firstDynamicOfKind(expr: Tree, kind: String): Option[String] = {
      val direct = extractDynamicSelects(expr)
        .find { case (path, _) => path.contains(kind) }
        .map(_._2)

      direct.orElse {
        expr match {
          case Ident(TermName(name)) =>
            defs.get(name).flatMap(rhs => firstDynamicOfKind(rhs, kind))
          case _ => None
        }
      }
    }

    /* ------------------------------------------------------------------
     * 3)  Traverse the tree and harvest   (message , channel)   pairs.
     * ------------------------------------------------------------------ */
    def loop(t: Tree): List[(String, String)] = t match
      // matches   Apply( Select( msgExpr, "send" ),  chanExpr :: rest )
      case Apply(Select(msgExpr, TermName("send")), chanExpr :: _) =>
        (for
          msg  <- firstDynamicOfKind(msgExpr,  "dispatch")
          chan <- firstDynamicOfKind(chanExpr, "channel")
        yield (msg, chan)).toList ++
          t.children.flatMap(loop)       // keep searching below

      case _ =>
        t.children.flatMap(loop)

      loop(tree).toSet
  }


  def main(args: Array[String]): Unit = {
    val code =
      """{
        |  val contextual$1: GenericDefinitions.ProcessingContext = GenericDefinitions.ProcessingContext.defaultCtx
        |  val msgAsk4Permission: GenericDefinitions.MessageEntity = GenericDefinitions.Keywords.dispatch.selectDynamic("AskPermission").:=[scala.Int](GenericDefinitions.Keywords.resource.selectDynamic("ProcessID").getValues.toList.head.toInt)
        |  val sent: scala.collection.immutable.Map[GenericDefinitions.MessageEntity, scala.Array[GenericDefinitions.ChannelEntity]] = msgAsk4Permission.send(GenericDefinitions.Keywords.channel.selectDynamic("ControlAction"))
        |  GenericDefinitions.Keywords.dispatch.selectDynamic("InformSinkProcess").send(GenericDefinitions.Keywords.channel.selectDynamic("Data"))
        |  GenericDefinitions.Keywords.resource.selectDynamic("sentNotification").:=[scala.Int](sent.toList.length)(GenericDefinitions.InputDataProcessor.given_TypeInfo_T[scala.Int](scala.reflect.ClassTag.apply[scala.Int](classOf[scala.Int])))
        |}""".stripMargin

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

    println(extractSendChannelPairs(code))
  }
