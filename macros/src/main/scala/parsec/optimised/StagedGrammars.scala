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
    case AcceptIf(p) =>
      Some(acceptIf(realElemType, p))

    //case Rep(g, t) => stage(g) match {
    //  case Some(f) => Some(fromParser(t, f).toListBuffer)
    //  case _ => None
    //}

    /**
     * the default case
     */
    case _ => None
  }
}
