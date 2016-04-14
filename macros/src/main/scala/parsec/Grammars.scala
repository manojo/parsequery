package parsec

import shapeless._
import ops.hlist._

trait Grammars {

  case class ~[+A, +B](_1: A, _2: B) {
    def map[C, D](f: A => C, g: B => D): C ~ D = new ~(f(_1), g(_2))
  }

  case class IndepTupleFunction[-T, -U, +R1, +R2](f1: T => R1, f2: U => R2)
      extends Function1[T ~ U, R1 ~ R2] {
    def apply(t: T ~ U) = t map (f1, f2)
  }

  type Id[T] = T

  /**
   * F has a map on it
   */
  abstract class Mappable[F[_]] {
    def map[T, U](f: T => U): F[T] => F[U]
  }

  implicit object IdMappable extends Mappable[Id] {
    def map[T, U](f: T => U) = (x: T) => f(x)
  }

  implicit object GrammarMappable extends Mappable[Grammar] {
    def map[T, U](f: T => U) = (g: Grammar[T]) => g map f
  }

  /**
   * inspired from
   * https://www.safaribooksonline.com/blog/2013/05/28/scala-type-classes-demystified/
   */
  implicit class FOps[F[_]: Mappable, T](t: F[T]) {
    val witness = implicitly[Mappable[F]]
    def map[U](f: T => U): F[U] = witness.map[T, U](f)(t)
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


  type Combine[T, R] = (R, T) => R
  /**
   * Just the usual fold grammar
   */
  abstract class Foldable[+T, F[_]: Mappable] { self =>

    def fold[R](z: R, combine: Combine[T, R]): F[R]

    /**
     * map. Pretty nice, cause we can forward the map
     * function over to the underlying parser, it's exactly
     * the same!
     */
    def map[U](f: T => U) = new Foldable[U, F] {
      def fold[R](z: R, combine: Combine[U, R]): F[R] = self.fold(
        z,
        (acc: R, elem: T) => combine(acc, f(elem))
      )
    }

    /**
     * filter
     */
    def filter(p: T => Boolean) = new Foldable[T, F] {
      def fold[R](z: R, comb: Combine[T, R]) = self.fold(
        z,
        (acc: R, elem: T) => if (p(elem)) comb(acc, elem) else acc
      )
    }

    /**
     * flatMap. It is unclear what semantics this should have for now
     * let's implement it later
     */
    /*def flatMap[U](f: T => CPSList[U, R]) = new Foldable[U, R] {

      def fold(z: R, comb: Combine[U, R]) = self.fold(
        z,
        (acc: R, elem: T) => {
          val nestedList = f(elem)
          nestedList.fold(acc, comb)
        }
      )
    }*/

    /**
     * partition
     * This will create code what will run through the original fold twice
     * once for the positive predicate, once for the negative.
     *
     * see the following related post: http://manojo.github.io/2015/03/03/staged-foldleft-partition/
     */
    def partition(p: T => Boolean): (Foldable[T, F], Foldable[T, F]) = {
      val trues = this filter p
      val falses = this filter (a => !p(a))
      (trues, falses)
    }

    /**
     * partfition, that produces a CPSList over `Either` instead of
     * two `CPSList`s. The important thing is to keep the one
     * CPSList abstraction.
     * This can be rewritten using `map`.
     * see the following related post: http://manojo.github.io/2015/03/12/staged-foldleft-groupby/
     */
    def partitionBis(p: T => Boolean) =
      this map (elem => if (p(elem)) Left(elem) else Right(elem))

    /**
     * groupWith
     * takes a function which computes some grouping property
     * does not create groups just yet, just propagates key-value pairs
     *
     * can be rewritten using `map`.
     * see the following related post: http://manojo.github.io/2015/03/12/staged-foldleft-groupby/
     */
    def groupWith[K](f: T => K): Foldable[(K, T), F] =
      this map (elem => (f(elem), elem))

    /**
     * utility functions that make it easier to write fold-like functions
     */
    def toListF[U >: T]: F[List[U]] = {
      import scala.collection.mutable.ListBuffer
      val folded: F[ListBuffer[U]] = self.fold[ListBuffer[U]](
        ListBuffer.empty[U],
        (acc: ListBuffer[U], t: U) => acc :+ t
      )
      folded map (_.toList)
    }

    def toSkipper: F[Unit] = self.fold((), (acc: Unit, _) => acc)
    def toLength: F[Int] = self.fold(0, (acc: Int, _) => acc + 1)

  }
  def fromGrammar[T](g: Grammar[T]): Foldable[T, Grammar] = new Foldable[T, Grammar] {
    def fold[R](z: R, combine: Combine[T, R]): Grammar[R] = RFold(g)(z, combine)
  }

  /**
   * a function of the form fs: Foldable[T, Id] => f(fs).fold(z, combine)
   */
  case class FoldableFolded[T, U, R, F[_]](
      f: Foldable[T, F] => Foldable[U, F],
      z: R, comb: (R, U) => R) extends Function1[Foldable[T, F], F[R]] {
    def apply(fs: Foldable[T, F]) = f(fs).fold(z, comb)
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
