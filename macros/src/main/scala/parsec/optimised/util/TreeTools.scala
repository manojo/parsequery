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
    assert(fun.tpe != null) //why?

    //assuming anon function
    val q"(..$params) => $body" = fun

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
      valDef(sym, changeOwner(arg, enclosingOwner, sym))
    }

    val old2new = params.map(_.symbol).zip(syms).toMap

    /**
     * perform the inlining itself
     */
    val inlined = {
      changeOwner(body, fun.symbol, enclosingOwner)
      typingTransform(body)((tree, api) => tree match {
        case Ident(_) if old2new.contains(tree.symbol) =>
            api.typecheck(q"${old2new(tree.symbol)}")
        case _ =>
          api.default(tree)
      })
    }

    q"""
      ..$vals
      $inlined
    """
  }
}
