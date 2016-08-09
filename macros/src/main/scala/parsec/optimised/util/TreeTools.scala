package parsec.optimised.util

import scala.reflect.macros.blackbox.Context

trait TreeTools {

  val c: Context
  import c.universe._
  import c.universe.definitions._
  import c.internal._, decorators._

  //scalastyle:off line.size.limit
  /**
   * creation of variables with symbols
   * should be the saner way of doing these.
   * @author densh
   * @see https://github.com/densh/scala-offheap/blob/master/macros/src/main/scala/offheap/internal/macros/Common.scala
   */
  //scalastyle:on line.size.limit

  def fresh(pre: String): TermName = TermName(c.freshName(pre))

  class SemiStable

  def freshVal(pre: String, tpe: Type, value: Tree, flags: FlagSet = NoFlags): ValDef = {
    val name = fresh(pre)
    val sym = enclosingOwner.newTermSymbol(name).setFlag(flags).setInfo(tpe)
    sym.updateAttachment(new SemiStable)
    val vd = valDef(sym, value)
    vd
  }

  def freshAnonVal(pre: String, tpe: Type): ValDef = {
    val name = fresh(pre)
    val sym = enclosingOwner.newTermSymbol(name).setFlag(NoFlags).setInfo(tpe)
    sym.updateAttachment(new SemiStable)
    val vd = valDef(sym)
    vd
  }

  def freshVar(pre: String, tpe: Type, value: Tree): ValDef =
    freshVal(pre, tpe, value, flags = Flag.MUTABLE)

  //scalastyle:off line.size.limit
  /**
   * A utility function for inlining function bodies
   * @author @densh @manojo
   * inspired from macrology
   * @see https://github.com/scalamacros/macrology201/blob/4dfbdf89704b3e91de056b5072c605028b25357a/macros/src/main/scala/Macros.scala#L41
   */
  //scalastyle:on line.size.limit

  def inline(fun: c.Tree, args: List[c.Tree]): c.Tree = {
    import c.internal._, decorators._

    /**
     * We might receive untyped functions here, if
     * they have been created using quotes
     */
    val tcheckedFun = c.typecheck(fun)
    //assert(tcheckedFun.tpe != null) //why?
    //val tcheckedFun = fun

    //assuming anon function
    val q"(..$params) => $body" = tcheckedFun

    /**
     * create fresh names and symbols for each
     * param in question
     */
    val syms = params map { param =>
      val q"..$_ val $_: ${tpe: Type} = $_" = param
      val trmName = TermName(c.freshName("temp"))
      enclosingOwner.newTermSymbol(trmName).setInfo(tpe)
    }

    /**
     * create valDefs which evaluate each of the args
     */
    val vals = syms.zip(args).map { case (sym, arg) =>
      valDef(sym, c.internal.changeOwner(arg, enclosingOwner, sym))
    }

    val old2new = params.map(_.symbol).zip(syms).toMap

    /**
     * perform the inlining itself
     */
    val inlined = {
      c.internal.changeOwner(body, tcheckedFun.symbol, enclosingOwner)
      typingTransform(body)((tree, api) => tree match {
        case Ident(_) if old2new.contains(tree.symbol) =>
            api.typecheck(q"${old2new(tree.symbol)}")
        case _ =>
          api.default(tree)
      })
    }

    splice(q"""
      ..$vals
      $inlined
    """)
  }

  /**
   * inspired by https://gist.github.com/retronym/10640845#file-macro2-scala
   * check out the gist for a detailed explanation of the technique
   */
  def splice(tree: c.Tree): c.Tree = {
    import internal._, decorators._
    tree.updateAttachment(macroutil.OrigOwnerAttachment(c.internal.enclosingOwner))
    q"_root_.macroutil.Splicer.changeOwner($tree)"
  }

  /**
   * In a given tree `t`, sym `a`, and syms `rest`,
   * find the biggest function application where only `a`
   * is involved (and none of `rest`) appear.
   *
   * Attention: we assume for now that the tree does not have too funky a shape
   * For instance (a + 2) + b + (a + 6) will not work as expected.
   */
  def largestIndepApplication(a: Symbol, rest: List[Symbol])(tree: c.Tree): SymbolUsageInfo = {
    type AllInfo = (Option[Tree], Boolean, Boolean)
    val existenceChecker = new VariableExistenceChecker(a, rest)

    /**
     * Walks through a list of trees, and accumulates
     * info on the connectedness of `a`
     * only one of `t :: args` may be allowed
     * to return a tree which has `(true, false)`.
     */
    def innerLoop(ls: List[Tree],
                  tmpRes: Option[Tree],
                  tmpSeenA: Boolean,
                  tmpRest: Boolean): AllInfo = ls match {
      case Nil => (tmpRes, tmpSeenA, tmpRest)
      case x :: xs => loop(x) match {
        /** only our sym has been seen */
        case (Some(t), true, false) => (tmpSeenA, tmpRest) match {

          /** Do we already hold a tree satisfying props? */
          //scalastyle:off return
          case (true, false) => return (None, true, false)
          //scalastyle:on return

          /** We don't, so let's keep this one */
          case (_, _) => innerLoop(xs, Some(t), true, false)
        }

        /** other syms are seen, that tree is useless */
        case _ => innerLoop(xs, tmpRes, tmpSeenA, tmpRest)
      }
    }

    def loop(tmpTree: c.Tree): AllInfo = {
      val (seenA, seenRest) = existenceChecker.getExistenceInfo(tmpTree)

      tmpTree match {
        case Apply(Select(t, _), args) => (seenA, seenRest) match {
          /**
           * we only need inspect deeply if all syms are present
           * specifically: only one of `t :: args` may be allowed
           * to return a tree which has `(true, false)`.
           */
          case (true, true)      => innerLoop(t :: args, None, false, false)
          case (seenA, seenRest) => (Some(tmpTree), seenA, seenRest)
        }

        case _ => (Some(tmpTree), seenA, seenRest)
      }
    }

    loop(tree) match {
      case (_, false, _)          => NotUsed
      case (Some(t), true, false) => IndepComponent(t)
      case _                      => NoInfo
    }
  }

  /**
   * Given tree `t`, sym `a` and syms `rest`, where `a` and `rest` are idents
   * does `t` contain `a`, and does it contain any of `rest`?
   * Returns both flags
   */
  class VariableExistenceChecker(a: Symbol, rest: List[Symbol]) extends Traverser {

    var seenA: Boolean = false
    var seenRest: Boolean = false

    override def traverse(tree: c.Tree) = tree match {
      case i @ Ident(_) =>
        if (i.symbol == a) { seenA = true }
        else if (rest.contains(i.symbol)) { seenRest = true }

      case _ => super.traverse(tree)
    }

    def getExistenceInfo(tree: c.Tree): (Boolean, Boolean) = {
      /**
       * resetting flags so that we can reuse the same checker
       * many times
       */
      seenA = false; seenRest = false
      traverse(tree)
      (seenA, seenRest)
    }
  }

  /**
   * Finds, in a given tree, the symbols of all
   * pattern match bindings.
   *
   * Example: given
   *    case (a, (b, c), d) => ...
   *
   * returns List(a, b, c, d)
   */
  class PatternBindings extends Traverser {
    import scala.collection.mutable.ListBuffer

    private val usedSymbols = ListBuffer[Symbol]()

    def inspect(tree: c.Tree): List[Symbol] = {
      traverse(tree)
      usedSymbols.toList
    }

    override def traverse(tree: c.Tree) = tree match {
      case d @ Bind(_, _) => usedSymbols += d.symbol
      case _ => super.traverse(tree)
    }
  }

  /**
   * An ADT representing info on a symbol in a function body
   */
  sealed abstract class SymbolUsageInfo
  case object NotUsed extends SymbolUsageInfo
  /** The independent component can't be an Ident */
  case class IndepComponent(t: c.Tree) extends SymbolUsageInfo
  case object NoInfo extends SymbolUsageInfo
}
