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

  /** combinators */
  case class Mapped(g: Grammar, f: Tree) extends Grammar

  case class Concat(l: Grammar, r: Grammar, t: Type) extends Grammar
  case class ConcatLeft(l: Grammar, r: Grammar, t: Type) extends Grammar
  case class ConcatRight(l: Grammar, r: Grammar, t: Type) extends Grammar
  case class Or(l: Grammar, r: Grammar, t: Type) extends Grammar

  /** base parsers */
  case class AcceptIf(f: Tree) extends Grammar
  case class PIdent(name: Ident) extends Grammar


  /**
   * Liftable and unliftable instances of a grammar
   */
  implicit val unliftGrammar: Unliftable[Grammar] = Unliftable {
    /** combinators */
    case q"${subg: Grammar}.map($arg)" => Mapped(subg, arg)
    case q"${l: Grammar} ~[${t: Type}] ${r: Grammar}" => Concat(l, r, t)
    case q"${l: Grammar} <~[${t: Type}] ${r: Grammar}" => ConcatLeft(l, r, t)
    case q"${l: Grammar} ~>[${t: Type}] ${r: Grammar}" => ConcatRight(l, r, t)
    case q"${l: Grammar} |[${t: Type}] ${r: Grammar}" => Or(l, r, t)

    /** base parsers */
    case q"$_.acceptIf($f)" => AcceptIf(f)
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
    /** combinators */
    case Mapped(g, f) => q"$g.map($f)"
    case Concat(l, r, t) => q"$l ~[$t] $r"
    case ConcatLeft(l, r, t) => q"$l <~[$t] $r"
    case ConcatRight(l, r, t) => q"$l ~>[$t] $r"
    case Or(l, r, t) => q"$l |[$t] $r"

    /** base parsers */
    case AcceptIf(f) => q"acceptIf($f)"
    case PIdent(tname) => q"$tname"
  }

}
