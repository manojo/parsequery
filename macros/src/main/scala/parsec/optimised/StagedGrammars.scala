package parsec.optimised

import scala.reflect.macros.blackbox.Context
import util.Zeroval

/**
 * This trait contains functionality to stage parser implementations
 * We are using the principle that we have unstaged functions
 * over staged types
 */
trait StagedGrammars
    extends GrammarTrees
    with ParserOps
    with ParseResultOps
    with Zeroval {

  val c: Context
  import c.universe._

  /* TODO: this is a bit hardcoded */
  val realElemType = typeOf[OptimisedParsers#Elem]

  /**
   * transforms a grammar into a "staged" parser
   * Rep[Input] => Rep[ParseResult[T]]
   * Since we work with trees in the the universe
   * we have Tree => Tree instead
   * We carry an `oldToNew` map that allows us to replace
   * named parsers with their modified names
   */
  def stage(g: Grammar)(implicit oldToNew: Map[Name, TermName]): Option[Parser] = g match {
    case AcceptIf(p) => Some(acceptIf(realElemType, p))
    case AcceptStr(s) => Some(acceptStr(s))
    case SuccessGrammar(t, elem) =>
      Some(mkParser(t, { in => mkSuccess(t, elem, in) }))

    /** TODO: should this be desugared before showing up here? */
    case SkipWs(t, g) => for (f <- stage(g)) yield skipWs(f)

    case Rep(g, t) => for (f <- stage(g)) yield rep(t, f)

    case Concat(l, r, t) => for {
      lp <- stage(l)
      rp <- stage(r)
    } yield (lp ~ rp)

    case ConcatLeft(l, r, t) => for {
      lp <- stage(l)
      rp <- stage(r)
    } yield (lp <~ rp)

    case ConcatRight(l, r, t) => for {
      lp <- stage(l)
      rp <- stage(r)
    } yield (lp ~> rp)

    case Or(l, r, t) => for {
      lp <- stage(l)
      rp <- stage(r)
    } yield (lp.or(t, rp))

    case Repsep(g, sep, t, u) => for {
      lp <- stage(g)
      rp <- stage(sep)
    } yield (repsep(lp, rp))

    /**
     * The `map` takes a `Tree => Tree` function
     * The naive `id` transformation involves
     * eta-expanding. TODO: inline functions which
     * can be.
     */
    case Mapped(g, f, t) => for (p <- stage(g)) yield p.map(t, t => q"$f($t)")

    /**
     * If we see a named parser, staging it boils down to calling the new
     * function name for this parser. This solves recursion issues right away
     * TODO: We could inline the definition of this named parser if it
     * is not recursive.
     */
    case PIdent(t @ Ident(name)) =>
      /**
       * `t.tpe` is `Parser[X]`
       * By this point, we know for sure this is true (the program would
       * have failed earlier otherwise)
       * To get `X` we use the `typeArgs` method.
       */
      val retType: Type = t.tpe.typeArgs.head

      val parser = mkParser(retType, { in => new ParseResult(retType) {
        val newName = oldToNew(name)

        val successResTerm  = TermName(c.freshName("res"))
        val successRes  = q"$successResTerm"
        val successRestTerm = TermName(c.freshName("rest"))
        val successRest = q"$successRestTerm"
        val failureRestTerm = TermName(c.freshName("rest"))
        val failureRest = q"$failureRestTerm"

        def apply(success: (Tree, Tree) => Tree, failure: Tree => Tree) = q"""
          $newName($in) match {
            case Success($successResTerm, $successRestTerm) => ${success(successRes, successRest)}
            case Failure(_, $failureRestTerm) => ${failure(failureRest)}
          }
        """
      }})

      Some(parser)

    /**
     * the default case
     */
    case _ => None
  }
}
