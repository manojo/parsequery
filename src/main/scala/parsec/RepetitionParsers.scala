package parsec

import scala.annotation.tailrec

/**
 * A simple Reducer for now
 */
 case class Reducer[A, R](z: R, combine: (R, A) => R)

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
 *    def foldLeft[A, B](z: B, comb: (B, A) => A)(xs: List[A]) : B
 *
 */

trait RepetitionParsers extends Parsers {

  def rep[T, R](p: Parser[T])(implicit red: Reducer[T, R]): Parser[R]
    = Repeat(new CPSListParser[T, R](p))

  /**
   * the repetition parser yields a `R` which is the result type
   * of a `CPSListParser`.
   *
   * We need to have a CPSList hanging around, as well as knowledge that
   * we can reduce it, which we take as an implicit
   * @TODO implicits so early seems like an overkill, will need to thinK
   * this through better.
   *
   * Design of `Reducer` inspired by fastparse, cf. `Repeater`
   * https://github.com/lihaoyi/fastparse/blob/master/fastparse/shared/src/main/scala/fastparse/ParserApi.scala
   */
  case class Repeat[T, R](p: CPSListParser[T, R])(implicit red: Reducer[T, R]) extends Parser[R] {
    def apply(in: Input) = p.fold(red.z, red.combine)(in)
  }

  /**
   * all functions on cpslists will have the F prefix (for fold)
   *
   * @TODO It would be nice to not have `R` popping out in the parameters here.
   * So that we only need specify the type at the very end.
   * an alternate design is to have `fold` carry it:
   * def fold[R](z: R, comb: Combine[A, R]): R
   */
  class CPSListParser[A, R](parser: Parser[A]) { self =>

    /**
     * a type alias for the combination function for
     * foldLeft
     * `A` is the type of elements that pass through the fold
     * `S` is the type that is eventually computed
     */
    type Combine[A, S] = (S, A) => S

    def fold(z: R, combine: Combine[A, R]): Parser[R] = Parser { in =>

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

    /**
     * mapF. Pretty nice, cause we can forward the map
     * function over to the underlying parser, it's exactly
     * the same!
     */
    def mapF[B](f: A => B) = new CPSListParser[B, R](parser map f)

    /**
     * filter
     */
    def filterF(p: A => Boolean) = new CPSListParser[A, R](parser) {

      override def fold(z: R, comb: Combine[A, R]) = self.fold(
        z,
        (acc: R, elem: A) => if (p(elem)) comb(acc, elem) else acc
      )
    }


    /**
     * flatMap. It is unclear what semantics this should have for now
     * let's implement it later
     */
    /*def flatMapF[B](f: A => CPSList[B, R]) = new CPSListParser[B, R] {

      def fold(z: R, comb: Combine[B, R]) = self.fold(
        z,
        (acc: R, elem: A) => {
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
    def partition(p: A => Boolean): (CPSListParser[A, R], CPSListParser[A, R]) = {
      val trues = this filterF p
      val falses = this filterF (a => !p(a))
      (trues, falses)
    }

    /**
     * partition, that produces a CPSList over `Either` instead of
     * two `CPSList`s. The important thing is to keep the one
     * CPSList abstraction.
     * This can be rewritten using `map`.
     * see the following related post: http://manojo.github.io/2015/03/12/staged-foldleft-groupby/
     */
    def partitionBis(p: A => Boolean) =
      this mapF (elem => if (p(elem)) Left(elem) else Right(elem))

    /**
     * groupWith
     * takes a function which computes some grouping property
     * does not create groups just yet, just propagates key-value pairs
     *
     * can be rewritten using `map`.
     * see the following related post: http://manojo.github.io/2015/03/12/staged-foldleft-groupby/
     */
    def groupWith[K](f: A => K): CPSListParser[(K, A), R] =
      this mapF (elem => (f(elem), elem))

  }
}
