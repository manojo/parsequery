package parsec.optimised

import scala.reflect.macros.blackbox.Context

/**
 * Contains macro-level trees representing grammars
 * along with lifting and unlifting for them
 */
trait GrammarTrees {
  val c: Context
  import c.universe._

  /**
   * cargo cult: TypeTrees work if they are declared at the top level
   * in a class. Otherwise they seem to result in a compiler error
   * Also, we are using type projection to access the inner Parser type
   * Should mostly work!
   */
  val parserType = typeOf[OptimisedParsers#Parser[_]]

  abstract class Grammar
  case class AcceptIf(f: Tree) extends Grammar
  case class Mapped(g: Grammar, f: Tree) extends Grammar
  case class PIdent(name: Ident) extends Grammar

  /**
   * Liftable and unliftable instances of a grammar
   */
  implicit val unliftGrammar: Unliftable[Grammar] = Unliftable {
    case q"${subg: Grammar}.map($arg)" => Mapped(subg, arg)
    case q"$_.accept($arg)" =>
      val temp = TermName(c.freshName("success"))
      AcceptIf(q"($temp: Elem) => $temp == $arg")
    /**
     * if we find any identifier, we make sure that it conforms to type
     * Parser before making a Grammar AST for it.
     */
    case q"${tname: Ident}" if tname.tpe <:< parserType => PIdent(tname)
  }

  implicit val liftGrammar: Liftable[Grammar] = Liftable {
    case Mapped(g, f) => q"$g.map($f)"
    case AcceptIf(f) => q"acceptIf($f)"
    case PIdent(tname) => q"$tname"
  }


}
