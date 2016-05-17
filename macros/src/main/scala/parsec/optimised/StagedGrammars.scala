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

//    case Mapped(g, f, t) => stage(g) match {
//      case Some(p) => Some(p.map(t, f))
//      case _ => None
//    }

    /**
     * the default case
     */
    case _ => None
  }
}
