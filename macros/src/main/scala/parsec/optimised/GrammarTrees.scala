package parsec.optimised

import util.TreeTools

import scala.reflect.macros.blackbox.Context

/**
 * Contains macro-level trees representing grammars
 * along with lifting and unlifting for them
 */
trait GrammarTrees { self: TreeTools =>

  val c: Context
  import c.universe._

  /**
   * cargo cult: TypeTrees work if they are declared at the top level
   * in a class. Otherwise they seem to result in a compiler error
   * Also, we are using type projection to access the inner Parser type
   * Should mostly work!
   */
  val parserType = typeOf[OptimisedParsers#Parser[_]]
  /* TODO: this is a bit hardcoded */
  val realElemType = typeOf[OptimisedParsers#Elem]

  abstract class Grammar(val tpe: Type)

  /** combinators */
  case class Mapped(g: Grammar, f: Tree, tpe2: Type) extends Grammar(tpe2)

  case class Concat(l: Grammar, r: Grammar, tpe2: Type)
    extends Grammar(appliedType(typeOf[Tuple2[_, _]], List(l.tpe, tpe2)))
  case class ConcatLeft(l: Grammar, r: Grammar, tpe2: Type) extends Grammar(l.tpe)
  case class ConcatRight(l: Grammar, r: Grammar, tpe2: Type) extends Grammar(tpe2)
  case class Or(l: Grammar, r: Grammar, tpe2: Type) extends Grammar(tpe2)
  case class Opt(g: Grammar) extends Grammar(appliedType(typeOf[Option[_]], List(g.tpe)))
  /**
   * we actually want the original tree
   * only the transformation phase will work on creating a new one
   * if needed
   */
  case class Rep(g: Grammar, tpe2: Type)
    extends Grammar(appliedType(typeOf[List[_]], tpe2))
  case class Repsep(g: Grammar, g2: Grammar, tpe2: Type, u: Type)
    extends Grammar(appliedType(typeOf[List[_]], tpe2))

  //case class RepFold(g: Grammar, t2: Type)(z: Tree, comb: Tree) extends Grammar

  /** base parsers */
  case class AcceptIf(path: List[Tree], p: Tree => Tree)
    extends Grammar(realElemType)
  case class AcceptStr(path: List[Tree], s: String)
    extends Grammar(typeOf[String])
  case class PIdent(name: Ident) extends Grammar(name.tpe)
  case class SuccessGrammar(path: List[Tree], tpe2: Type, elem: Tree) extends Grammar(tpe2)
  case class Number(path: List[Tree]) extends Grammar(typeOf[Int])
  case class DoubleGrammar(path: List[Tree]) extends Grammar(typeOf[Double])
  case class StringLiteral(path: List[Tree]) extends Grammar(typeOf[String])

  /* TODO: should desugar this later */
  case class SkipWs(path: List[Tree], tpe2: Type, g: Grammar) extends Grammar(tpe2)

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
    case q"$_.opt[${t: Type}](${g: Grammar})" => Opt(g)

    /** base parsers */
    case q"(..$p).acceptIf($f)"         => AcceptIf(p, elem => q"$f($elem)")
    case q"(..$p).accept(${arg: Char})" => AcceptIf(p, elem => q"$elem == $arg")
    case q"(..$p).letter"               => AcceptIf(p, elem => q"$elem.isLetter")
    case q"(..$p).digit"                => AcceptIf(p, elem => q"$elem.isDigit")
    case q"(..$p).digit2Int" =>

      val argTerm = TermName(c.freshName("arg"))
      val arg = q"$argTerm"

      val q"${d: Grammar}" = q"(..$p).digit"
      val f = q"($arg: Char) => {($arg - '0').toInt}"

      val intType: Type = typeOf[Int]

      val q"${g: Grammar}" = q"$d.map[$intType]($f)"
      g

    case q"(..$p).number"        => Number(p)
    case q"(..$p).stringLiteral" => StringLiteral(p)
    case q"(..$p).double"        => DoubleGrammar(p)

    case q"(..$p).accept(${arg: String})"     => AcceptStr(p, arg)
    case q"(..$p).success[${t: Type}]($elem)" => SuccessGrammar(p, t, elem)

    case q"(..$p).skipWs[${t: Type}](${g: Grammar})" => SkipWs(p, t, g)

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
    case Opt(g) => q"opt[${g.tpe}]($g)"

    /** base parsers */
    case AcceptIf(p, pred) =>
      val temp = TermName(c.freshName("temp"))
      q"(..$p).acceptIf(($temp: Elem) => ${pred(q"$temp")})"

    case AcceptStr(p, s) => q"(..$p).accept($s)"
    case SuccessGrammar(p, t, elem) => q"(..$p).success[$t]($elem)"
    case PIdent(tname) => q"$tname"
    case SkipWs(p, t, g) => q"(..$p).skipWs[$t]($g)"

    case Number(p) => q"(..$p).number"
    case DoubleGrammar(p) => q"(..$p).double"
    case StringLiteral(p) => q"(..$p).stringLiteral"
  }
}
