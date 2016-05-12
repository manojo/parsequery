package parsec

import scala.annotation.tailrec
import util.Foldable
import util.Functors._
import util.MyList
import util.{Nil => Nil, Cons => Cons}

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

  implicit object ParserFunctor extends Functor[Parser] {
    def map[T, U](f: T => U) = (g: Parser[T]) => g map f
  }

  /**
   * a type alias for the combination function for
   * foldLeft
   * `T` is the type of elements that pass through the fold
   * `R` is the type that is eventually computed
   */
  type Combine[T, R] = (R, T) => R

  def repFold[T](p: Parser[T]): FoldParser[T] = fromParser(p)
  def rep[T](p: Parser[T]): Parser[MyList[T]] = repFold(p).toMyListF

  /**
   * repeatedly parses `parser`, interspersed with the `sep` parser
   * we must bake this in as a specific `FoldParser`, because we want to
   * use the `combine` function for the first parse result as well.
   * TODO: could `sep` always be a `Parser[Unit]`?
   */
  def repsepFold[T, U](parser: Parser[T], sep: => Parser[U]): FoldParser[T] = {
    new FoldParser[T] {
      def fold[R](z: R, combine: Combine[T, R]): Parser[R] = Parser { in =>

        /* The loop runs over the composed parser */
        @tailrec
        def loop(curIn: Input, curRes: R): ParseResult[R] = (sep ~> parser)(curIn) match {
          case Success(res, rest) => loop(rest, combine(curRes, res))
          /* The rest is where we started failing*/
          case Failure(_, _) => Success(curRes, curIn)
        }

        /**
         * We need to run `parser` once, for getting the first result
         */
        parser(in) match {
          case Success(res, rest) => loop(rest, combine(z, res))
          case Failure(_, _) => Success(z, in)
        }
      }
    }
  }

  def repsep[T, U](parser: Parser[T], sep: => Parser[U]): Parser[MyList[T]] =
    repsepFold(parser, sep).toMyListF

  /**
   * the repetition parser yields a `R` which is the result type
   * of a `FoldParser`.
   *
   * We need to have a CPSList hanging around
   */

  /* create a `FoldParser` given a parser */
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
  abstract class FoldParser[+T] extends Foldable[T, Parser]

  /**
   * ops on folding chars
   */
  implicit class CharFoldOps(cFold: FoldParser[Char]) {

    def toStringParser: Parser[String] = {
      import scala.collection.mutable.StringBuilder
      cFold.fold[StringBuilder](
        StringBuilder.newBuilder,
        (acc: StringBuilder, c: Char) => acc append c
      ).map(_.toString)
    }
  }

}
