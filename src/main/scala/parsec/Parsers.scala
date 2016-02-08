package parsec

import scala.annotation.tailrec
import scala.collection.mutable.ListBuffer

/**
 * An alternative implementation of (unstaged) parser combinators
 * based on the usual, but also a bit different for the concatenation
 */

trait Parsers {

  type Elem
  type Input = Reader[Elem]

  /**
   * We seek to write a function transform, that takes
   * a `Parser` (Actually a concat parser), a set of indices
   * and adds the arrows around the concats as required.
   * We will first do it in a slightly untyped fashion.
   */

  abstract class Parser[+T] extends (Input => ParseResult[T]) {

    /**
     * The flatMap operation
     */
    def flatMap[U](f: T => Parser[U]) = Parser[U] { input =>
      this(input) flatMapWithNext { res => rdr => f(res)(rdr) }
    }

    def >>[U](f: T => Parser[U]) = flatMap(f)

    /**
     * The concat operation
     */
    def ~[U](that: => Parser[U]): Parser[(T, U)] =
      for (l <- this; r <- that) yield (l, r)

    /**
     * get right hand side result
     */
    def ~>[U](that: => Parser[U]): Parser[U] =
      this flatMap { l => that }

    /**
     * get left hand side result
     */
    def <~[U](that: => Parser[U]): Parser[T] =
      for (l <- this; r <- that) yield l

    /**
     * The map operation
     */
    def map[U](f: T => U) = Parser[U] { input =>
      this(input) map f
    }

    /**
     * alternation, aka the beast
     * Note: actually, it's not much of a beast nomore!
     */
    def | [U >: T](that: => Parser[U]) = Parser[U] { input =>
      this(input) orElse that(input)
    }

  }

  /**
   * companion object for apply function
   */
  object Parser {
    def apply[T](f: Input => ParseResult[T]) = new Parser[T] {
      def apply(in: Input) = f(in)
    }
  }

  /**
   * The simplest possible combinator
   */
  def acceptIf(p: Elem => Boolean): Parser[Elem] = Parser { in =>
    if (in.atEnd) Failure("oh noes!", in)
    else if (p(in.first)) Success(in.first, in.rest)
    else Failure("on noes!", in)
  }

  def accept(e: Elem) = acceptIf(_ == e)

  /**
   * the repetition parser yields lists for now. We should
   * change this to yield a Foldable at some point.
   */
  def rep[T](p: Parser[T]): Parser[List[T]] = Parser { in =>

    @tailrec
    def loop(curIn: Input, curRes: ListBuffer[T]): ParseResult[ListBuffer[T]] =
      p(curIn) match {
        case Success(res, rest) => loop(rest, curRes :+ res)

        /**
         * The rest is where we started failing
         */
        case Failure(_, _) => Success(curRes, curIn)
      }

    loop(in, ListBuffer.empty) map (_.toList)
  }



  /**
   * ParseResult, follows the implementation in the standard library
   */

  sealed abstract class ParseResult[+T] {

    def map[U](f: T => U): ParseResult[U] = this match {
      case Success(elem, rest) => Success(f(elem), rest)
      case f @ Failure(_, _) => f
    }

    def orElse[U >: T](that: => ParseResult[U]): ParseResult[U] = this match {
      case s @ Success(_, _) => s
      case _ => that
    }

    def flatMapWithNext[U](f: T => Input => ParseResult[U]): ParseResult[U] = this match {
      case Success(t, rest) => f(t)(rest)
      case f @ Failure(_, _) => f
    }

  }

  case class Success[+T](elem: T, rest: Input) extends ParseResult[T]
  case class Failure(errMessage: String, rest: Input) extends ParseResult[Nothing]

}
