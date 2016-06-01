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
  case class Repsep(g: Grammar, g2: Grammar, t: Type, u: Type) extends Grammar
  //case class RepFold(g: Grammar, t: Type)(z: Tree, comb: Tree) extends Grammar

  /** base parsers */
  case class AcceptIf(p: Tree => Tree) extends Grammar
  case class AcceptStr(s: String) extends Grammar
  case class PIdent(name: Ident) extends Grammar
  case class SuccessGrammar(t: Type, elem: Tree) extends Grammar

  /* TODO: should desugar this later */
  case class SkipWs(t: Type, g: Grammar) extends Grammar

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
    case q"$_.repsep[${t: Type}, ${u: Type}](${g: Grammar}, ${sep: Grammar})" =>
      Repsep(g, sep, t, u)

    /** base parsers */
    case q"$_.acceptIf($f)" => AcceptIf(elem => q"$f($elem)")
    case q"$_.accept(${arg: Char})" => AcceptIf(elem => q"$elem == $arg")
    case q"$_.letter" => AcceptIf(elem => q"$elem.isLetter")
    case q"$_.digit" => AcceptIf(elem => q"$elem.isDigit")

    case q"$_.accept(${arg: String})" => AcceptStr(arg)
    case q"$_.success[${t: Type}]($elem)" => SuccessGrammar(t, elem)

    case q"$_.skipWs[${t: Type}](${g: Grammar})" => SkipWs(t, g)

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
    case Repsep(g, sep, t, u) => q"repsep[$t, $u]($g, $sep)"

    /** base parsers */
    case AcceptIf(p) =>
      val temp = TermName(c.freshName("temp"))
      q"acceptIf(($temp: Elem) => ${p(q"$temp")})"

    case AcceptStr(s) => q"accept($s)"
    case PIdent(tname) => q"$tname"
    case SkipWs(t, g) => q"skipWs[$t]($g)"
  }
}
