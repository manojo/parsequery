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
  case class Mapped(g: Grammar, f: Tree, t: Type) extends Grammar

  case class Concat(l: Grammar, r: Grammar, t: Type) extends Grammar
  case class ConcatLeft(l: Grammar, r: Grammar, t: Type) extends Grammar
  case class ConcatRight(l: Grammar, r: Grammar, t: Type) extends Grammar
  case class Or(l: Grammar, r: Grammar, t: Type) extends Grammar
  /**
   * we actually want the original tree
   * only the transformation phase will work on creating a new one
   * if needed
   */
  case class Rep(g: Grammar, t: Type) extends Grammar
  //case class RepFold(g: Grammar, t: Type)(z: Tree, comb: Tree) extends Grammar

  /** base parsers */
  case class AcceptIf(f: Tree) extends Grammar
  case class PIdent(name: Ident) extends Grammar


  /**
   * Liftable and unliftable instances of a grammar
   */
  implicit val unliftGrammar: Unliftable[Grammar] = Unliftable {
    /** combinators */
    case q"${subg: Grammar}.map[${t: Type}]($arg)" => Mapped(subg, arg, t)
    case q"${l: Grammar} ~[${t: Type}] ${r: Grammar}" => Concat(l, r, t)
    case q"${l: Grammar} <~[${t: Type}] ${r: Grammar}" => ConcatLeft(l, r, t)
    case q"${l: Grammar} ~>[${t: Type}] ${r: Grammar}" => ConcatRight(l, r, t)
    case q"${l: Grammar} |[${t: Type}] ${r: Grammar}" => Or(l, r, t)
    case q"$_.rep[${t: Type}](${g: Grammar})" => Rep(g, t)

    /** base parsers */
    case q"$_.acceptIf($f)" => AcceptIf(f)
    case q"$_.accept($arg)" =>
      val temp = TermName(c.freshName("temp"))
      AcceptIf(q"($temp: Elem) => $temp == $arg")

    case q"$_.letter" =>
      val temp = TermName(c.freshName("temp"))
      AcceptIf(q"($temp: Elem) => $temp.isLetter")

    case q"$_.digit" =>
      val temp = TermName(c.freshName("temp"))
      AcceptIf(q"($temp: Elem) => $temp.isDigit")

    /**
     * if we find any identifier, we make sure that it conforms to type
     * Parser before making a Grammar AST for it.
     */
    case q"${tname: Ident}" if tname.tpe <:< parserType => PIdent(tname)
  }

  implicit val liftGrammar: Liftable[Grammar] = Liftable {
    /** combinators */
    case Mapped(g, f, t) => q"$g.map[$t]($f)"
    case Concat(l, r, t) => q"$l ~[$t] $r"
    case ConcatLeft(l, r, t) => q"$l <~[$t] $r"
    case ConcatRight(l, r, t) => q"$l ~>[$t] $r"
    case Or(l, r, t) => q"$l |[$t] $r"
    case Rep(g, t) => q"rep[$t]($g)"

    /** base parsers */
    case AcceptIf(f) => q"acceptIf($f)"
    case PIdent(tname) => q"$tname"
  }

}
