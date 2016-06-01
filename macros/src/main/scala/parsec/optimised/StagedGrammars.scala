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
   *
   */
  def stage(g: Grammar): Option[Parser] = g match {
    case AcceptIf(p) => Some(acceptIf(realElemType, p))
    case AcceptStr(s) => Some(acceptStr(s))

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
     * the default case
     */
    case _ => None
  }
}
