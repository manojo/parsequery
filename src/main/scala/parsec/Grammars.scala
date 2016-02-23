package parsec

import shapeless._
import ops.hlist._

trait Grammars {

  //type MyList[T] = RepeatGrammar[T]

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
    def rep: Grammar[FoldGrammar[T]] = Repeat(fromGrammar(this))
    def map[U](f: T => U): Grammar[U] = Mapped(this, f)
    def |[U >: T](that: Grammar[U]): Grammar[U] = Or(this, that)
  }

  case class SimpleGrammar[+T]() extends Grammar[T]

  case class Concat[T, U](l: Grammar[T], r: Grammar[U]) extends Grammar[T ~ U]
  case class Or[T](l: Grammar[T], r: Grammar[T]) extends Grammar[T]

  case class RFold[T, R](g: Grammar[T])(z: R, comb: (R, T) => R)
    extends Grammar[R]
  case class Repeat[T](fg: FoldGrammar[T]) extends Grammar[FoldGrammar[T]]

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
      case Repeat(gInner) => m.f match {
        case fgf: FGFold[_, T] => gInner.fold(fgf.z, fgf.comb)
        case fg2fg: FG2FG[_, _] => Repeat(fg2fg.f(gInner))
        case _ => g
      }



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
  abstract class FoldGrammar[+T] { self =>

    def fold[R](z: R, combine: Combine[T, R]): Grammar[R]

    /**
     * map. Pretty nice, cause we can forward the map
     * function over to the underlying parser, it's exactly
     * the same!
     */
    def map[U](f: T => U) = new FoldGrammar[U] {
      def fold[R](z: R, combine: Combine[U, R]): Grammar[R] = self.fold(
        z,
        (acc: R, elem: T) => combine(acc, f(elem))
      )
    }

    /**
     * filter
     */
    def filter(p: T => Boolean) = new FoldGrammar[T] {
      def fold[R](z: R, comb: Combine[T, R]) = self.fold(
        z,
        (acc: R, elem: T) => if (p(elem)) comb(acc, elem) else acc
      )
    }

    /**
     * flatMap. It is unclear what semantics this should have for now
     * let's implement it later
     */
    /*def flatMap[U](f: T => CPSList[U, R]) = new FoldGrammar[U, R] {

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
    def partition(p: T => Boolean): (FoldGrammar[T], FoldGrammar[T]) = {
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
    def groupWith[K](f: T => K): FoldGrammar[(K, T)] =
      this map (elem => (f(elem), elem))

    /**
     * utility functions that make it easier to write fold-like functions
     */
    def toListGrammar: Grammar[List[T]] = {
      import scala.collection.mutable.ListBuffer
      self.fold[ListBuffer[T]](
        ListBuffer.empty[T],
        (acc: ListBuffer[T], t: T) => acc :+ t
      ).map(_.toList)
    }

    def toSkipper: Grammar[Unit] = self.fold((), (acc: Unit, _) => acc)
    def toLength: Grammar[Int] = self.fold(0, (acc: Int, _) => acc + 1)

  }

  def fromGrammar[T](g: Grammar[T]): FoldGrammar[T] = new FoldGrammar[T] {
    def fold[R](z: R, combine: Combine[T, R]): Grammar[R] = RFold(g)(z, combine) /*{ in =>

      @tailrec
      def loop(curIn: Input, curRes: R): ParseResult[R] = parser(curIn) match {
        case Success(res, rest) => loop(rest, combine(curRes, res))

        /**
         * The rest is where we started failing
         */
        case Failure(_, _) => Success(curRes, curIn)
      }

      loop(in, z)
    }*/
  }

  case class FGFold[-T, R](z: R, comb: (R, T) => R)
      extends Function1[FoldGrammar[T], R] {
    // A DUMMY FUNCTION!!!!!
    def apply(f: FoldGrammar[T]) = ??? //f.fold(z, comb)
  }

  case class FG2FG[T, U](f: FoldGrammar[T] => FoldGrammar[U])
      extends Function1[FoldGrammar[T], FoldGrammar[U]] {

    def apply(fg: FoldGrammar[T]) = f(fg)
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

    val repG: G[Int ~ Int ~ Int] = (g1 ~ g2.rep ~ g3) map (
      IndepTupleFunction(
        IndepTupleFunction(
          (i: Int) => i + 1,
          FGFold[Char, Int](0, (acc, e) => acc + 1)
        ),
        (i: Int) => i + 5
      )
    )

    println(repG)
    println(transform(repG))


    val repG2: G[Int ~ FoldGrammar[Char] ~ Int] = (g1 ~ g2.rep ~ g3) map (
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

  }
}
