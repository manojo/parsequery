package parsec

import scala.annotation.tailrec

/**
 * Something potentially beautiful:
 *
 * Consider the usual `rep` function on parsers:
 *
 *    rep: Parser[T] => Parser[List[T]]
 *
 * We can abstract over the return type, as essentially we are
 * folding a parser into a list:
 *
 *    rep: Parser[T] => (z: R, combine: (R, T) => R) => Parser[R]
 *
 * In curried terms, this is a function that takes a parser, and
 * returns a late parser (of a list):
 *
 *    rep: Parser[T] => CPSListParser[T]
 *
 * The signature of CPSListParser is different from that of a usual CPSList:
 *
 *    CPSList[T] => forall R. (z: R, combine: (R, T) => R) => R
 *
 * Note the difference in the final return type.
 * But we also need to be able to call methods on parser on a rep(p). Therefore
 * `CPSListParser` has to be have parser operations too! In monadic terms:
 * `CPSListParser` is a cps list monad stacked on top of a parser monad.
 *
 * This should help us get amazing fold fusion goodness on parser combinators!
 *
 * inspired from:
 * https://github.com/manojo/staged-fold-fusion/blob/master/src/main/scala/barbedwire/CPSList.scala
 *
 * the type signature of foldLeft is
 *    def foldLeft[T, R](z: R, comb: (R, T) => T)(xs: List[T]) : R
 *
 */

trait RepetitionParsers extends Parsers {

  /**
   * a type alias for the combination function for
   * foldLeft
   * `T` is the type of elements that pass through the fold
   * `R` is the type that is eventually computed
   */
  type Combine[T, R] = (R, T) => R

  def rep[T](p: Parser[T]) = fromParser(p)

  /**
   * the repetition parser yields a `R` which is the result type
   * of a `FoldParser`.
   *
   * We need to have a CPSList hanging around
   */

  /**
   * create a FoldParser given a parser
   */
  def fromParser[T](parser: Parser[T]): FoldParser[T] = new FoldParser[T] {
    def fold[R](z: R, combine: Combine[T, R]): Parser[R] = Parser { in =>

      @tailrec
      def loop(curIn: Input, curRes: R): ParseResult[R] = parser(curIn) match {
        case Success(res, rest) => loop(rest, combine(curRes, res))

        /**
         * The rest is where we started failing
         */
        case Failure(_, _) => Success(curRes, curIn)
      }

      loop(in, z)
    }
  }

  /**
   * Just the usual fold parser
   */
  abstract class FoldParser[T] { self =>

    def fold[R](z: R, combine: Combine[T, R]): Parser[R]

    /**
     * map. Pretty nice, cause we can forward the map
     * function over to the underlying parser, it's exactly
     * the same!
     */
    def map[U](f: T => U) = new FoldParser[U] {
      def fold[R](z: R, combine: Combine[U, R]): Parser[R] = self.fold(
        z,
        (acc: R, elem: T) => combine(acc, f(elem))
      )
    }

    /**
     * filter
     */
    def filter(p: T => Boolean) = new FoldParser[T] {
      def fold[R](z: R, comb: Combine[T, R]) = self.fold(
        z,
        (acc: R, elem: T) => if (p(elem)) comb(acc, elem) else acc
      )
    }

    /**
     * flatMap. It is unclear what semantics this should have for now
     * let's implement it later
     */
    /*def flatMapF[U](f: T => CPSList[U, R]) = new FoldParser[U, R] {

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
    def partition(p: T => Boolean): (FoldParser[T], FoldParser[T]) = {
      val trues = this filter p
      val falses = this filter (a => !p(a))
      (trues, falses)
    }

    /**
     * partition, that produces a CPSList over `Either` instead of
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
    def groupWith[K](f: T => K): FoldParser[(K, T)] =
      this map (elem => (f(elem), elem))

    /**
     * We can now concatenate a parser!
     */
    def ~[U](that: Parser[U]): FoldConcatParser[T, U] = new FoldConcatParser[T, U] {
      def fold[R](z: R, combine: Combine[T, R]): Parser[(R, U)] =
        self.fold(z, combine) ~ that
    }

    /**
     * we can concatenate another rep parser too!
     */
    def ~[U](that: FoldParser[U]) = new FoldFoldParser[T, U] {
      def fold[R1, R2](f1: FoldFunction[T, R1], f2: FoldFunction[U, R2]): Parser[(R1, R2)] =
        self.fold[R1](f1._1, f1._2) ~ that.fold[R2](f2._1, f2._2)
    }
  }

  /**
   * We need to be able to compose rep(a) ~ b in a way such that we can provide the fold function
   * after the b. This needs to extend to any amount of concatenations. Should be do-able
   * with this one extra class
   */
  abstract class FoldConcatParser[T, U] { self =>

    /**
     * the fold function still only holds for the first parser,
     * i.e. we only fold over `T`
     */
    def fold[R](z: R, combine: Combine[T, R]): Parser[(R, U)]

    /**
     * Concatenation must become right associative now, because we don't
     * want a `FoldConcatParser[((T, U), V)]
     */
    def ~[V](that: Parser[V]): FoldConcatParser[T, (U, V)] =  {
      new FoldConcatParser[T, (U, V)] {
        def fold[R](z: R, combine: Combine[T, R]): Parser[(R, (U, V))] = {
          (self.fold(z, combine) ~ that) map {
            case ((r, u), u2) => (r, (u, u2))
          }
        }
      }
    }
  }

  /**
   * We need to be able to compose rep(a) ~ rep(b) as well.
   */
  abstract class FoldFoldParser[T, U] { self =>

    type FoldFunction[A, R] = (R, Combine[A, R])

    /**
     * there are two fold functions now passed
     */
    def fold[R1, R2](f1: FoldFunction[T, R1], f2: FoldFunction[U, R2]): Parser[(R1, R2)]
  }

  /**
   * some handy folders
   */
  def listFolder[T] = (List[T](), (acc: List[T], t: T) => acc :+ t)
  def lengthFolder[T] = (0, (acc: Int, t: T) => acc + 1)

  import scala.collection.mutable.ArrayBuffer
  def arrayBufFolder[T] = (
    ArrayBuffer.empty[T],
    (acc: ArrayBuffer[T], t: T) => acc :+ t
  )

}
