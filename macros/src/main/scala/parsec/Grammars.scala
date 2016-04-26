package parsec

import shapeless._
import ops.hlist._

import util.Functors._
import util.Functors.{Id => Id}
import util.Foldable

trait Grammars {

  implicit object GrammarFunctor extends Functor[Grammar] {
    def map[T, U](f: T => U) = (g: Grammar[T]) => g map f
  }

  case class ~[+A, +B](_1: A, _2: B) {
    def map[C, D](f: A => C, g: B => D): C ~ D = new ~(f(_1), g(_2))
  }

  case class IndepTupleFunction[-T, -U, +R1, +R2](f1: T => R1, f2: U => R2)
      extends Function1[T ~ U, R1 ~ R2] {
    def apply(t: T ~ U) = t map (f1, f2)
  }

  abstract class Grammar[+T] {

    /**
     * right-associative append is better?
     */
    def ~[U](that: Grammar[U]): Grammar[T ~ U] = Concat(this, that)
    def rep: Grammar[Foldable[T, Id]] = Repeat(this)
    def map[U](f: T => U): Grammar[U] = Mapped(this, f)
    def |[U >: T](that: Grammar[U]): Grammar[U] = Or(this, that)
  }

  case class SimpleGrammar[+T]() extends Grammar[T]

  case class Concat[T, U](l: Grammar[T], r: Grammar[U]) extends Grammar[T ~ U]
  case class Or[T](l: Grammar[T], r: Grammar[T]) extends Grammar[T]

  case class RFold[T, R](g: Grammar[T])(z: R, comb: (R, T) => R)
    extends Grammar[R]
  case class Repeat[T](g: Grammar[T]) extends Grammar[Foldable[T, Id]]

  case class Mapped[T, U](g: Grammar[T], f: T => U) extends Grammar[U]


  /**
   * Amazing find thanks to @sjrd. turns out you can pattern match on type
   * parameters, and can __capture__ them using lower case names
   */
  def transform[T](g: Grammar[T]): Grammar[T] = g match {

    case m: Mapped[_, T] => m.g match {

      /**
       * A `map` over a `concat`, where the function itself is a `map` on the
       * underlying tuple.
       *
       * [[ (a ~ b) map (p map (f1, f2)) ]] => [[ (a map f1) ]] ~ [[ (b map f2) ]]
       */
      case Concat(l, r) => m.f match {
        case f: IndepTupleFunction[_, _, _, _] =>
          transform(l map f.f1) ~ transform(r map f.f2)
        case _ => g
      }

      /**
       * A `map` over a `rep` parser
       * [[ rep(a) map f ]] ==> [[ repFold(a)(z)(f) ]]
       */
//      case Repeat(gInner) => m.f match {
//        case fgf: FoldableFolded[_, _, _ ,Id] =>
//          fgf.f(fromGrammar(transform(g))).fold(fgf.z, fgf.comb)
//        case _ => g
//      }

      case _ => g
    }
    case _ => g
  }

  trait TypeTest {

    type G[T] = Grammar[T]

    def concat2(g1: G[Int], g2: G[Char]): G[Int ~ Char] = g1 ~ g2

    def concatN(
      g1: G[Int],
      g2: G[Char],
      g3: G[Int],
      g4: G[Char]): G[Int ~ Char ~ Int ~ Char] = g1 ~ g2 ~ g3 ~ g4
  }

  def fromGrammar[T](g: Grammar[T]): Foldable[T, Grammar] = {
    new Foldable[T, Grammar] {
      def fold[R](z: R, combine: (R, T) => R): Grammar[R] =
        RFold(g)(z, combine)
    }
  }
}



object HelloGrammars extends Grammars {

  def main(args: Array[String]): Unit = {
    type G[T] = Grammar[T]

    val g1 = SimpleGrammar[Int]()
    val g2 = SimpleGrammar[Char]()
    val g3 = SimpleGrammar[Int]()
    val g4 = SimpleGrammar[Char]()

    val g = (g1 ~ g2 ~ g3 ~ g4).map(
      IndepTupleFunction(
        IndepTupleFunction(
          IndepTupleFunction(
            (i: Int) => i + 1,
            (c: Char) => c + 23
          ),
          (i: Int) => i + 5
        ),
        (c: Char) => c - '0'
      )
    )

    println(g)
    println(transform(g))
/*
    val repG: G[Int ~ Int ~ Int] = (g1 ~ g2.rep ~ g3) map (
      IndepTupleFunction(
        IndepTupleFunction(
          (i: Int) => i + 1,
          FGFold[Char, Int, Id](0, (acc, e) => acc + 1)
        ),
        (i: Int) => i + 5
      )
    )

    println(repG)
    println(transform(repG))
*/
/*
    val repG2: G[Int ~ Foldable[Char] ~ Int] = (g1 ~ g2.rep ~ g3) map (
      IndepTupleFunction(
        IndepTupleFunction(
          (i: Int) => i + 1,
          FG2FG[Char, Char](f => f.filter(_ > 'a'))
        ),
        (i: Int) => i + 5
      )
    )

    println(repG2)
    println(transform(repG2))
*/
  }
}
