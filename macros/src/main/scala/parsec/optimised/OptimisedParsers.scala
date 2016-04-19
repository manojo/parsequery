package parsec.optimised

import parsec._
import scala.language.experimental.macros
import scala.reflect.macros.blackbox.Context

trait OptimisedParsers extends CharParsers {

  def optimise[T](parserBlock: => this.Parser[T]): Parser[T] =
    macro OptimisedParsersImpl.optimise

}

class OptimisedParsersImpl(val c: Context) extends GrammarTrees {
  import c.universe._

  /**
   * Takes a list of statements that represent code that is
   * declared in the parser block, and constructs a list of
   * `Grammar` trees for all parsers declared in the block
   * including the final one.
   *
   * A parser block can only contain (for now) parser declarations
   *
   */
  def collectDeclaredParsers(statements: List[c.Tree]): List[Grammar] = {
    val (stmts, finalParser) = (statements.init, statements.last)

    val parserDefinitions: List[Grammar] = stmts collect {
      case q"def $name[..$tparams]: ${d: Type} = ${g: Grammar}" if d <:< parserType => g
      case q"val $name: ${d: Type} = ${g: Grammar}" if d <:< parserType => g
      case stmt @ _ => c.abort(
        c.enclosingPosition,
        s"""only parser definitions are allowed in the `optimise` scope.
        |Yet we got the following:
        |${showCode(stmt)}
        |""".stripMargin
      )
    }

    val q"${finalG: Grammar}" = finalParser
    parserDefinitions :+ finalG
  }

  /**
   * This method is the macro entry point. TODO: it should be typed
   * so that it is a c.Expr[Parser[T]].
   */
  def optimise(parserBlock: c.Tree) = parserBlock match {
    case q"{..$statements}" =>
      val grammars = collectDeclaredParsers(statements)

      /**
       * for now we return only the last statement
       * aka we assume only one statement
       */
      liftGrammar(grammars.last)

    case _ =>
      println(showCode(parserBlock))
      c.abort(c.enclosingPosition, "the body does not match anything we expect")
  }
}
