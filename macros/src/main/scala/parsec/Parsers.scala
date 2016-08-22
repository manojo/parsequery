package parsec

import scala.annotation.tailrec

/**
 * An alternative implementation of (unstaged) parser combinators
 * based on the usual, but also a bit different for the concatenation
 */
trait Parsers extends java.io.Serializable {

  type Elem
  type Input = Reader[Elem]

  /**
   * We seek to write a function transform, that takes
   * a `Parser` (Actually a concat parser), a set of indices
   * and adds the arrows around the concats as required.
   * We will first do it in a slightly untyped fashion.
   */

  abstract class Parser[+T]
      extends (Input => ParseResult[T])
      with java.io.Serializable {

    /**
     * The flatMap operation
     */
    def flatMap[U](f: T => Parser[U]): Parser[U] = FlatMapped(this, f)

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
    def map[U](f: T => U): Parser[U] = Mapped(this, f)

    /**
     * alternation, aka the beast
     * Note: actually, it's not much of a beast nomore!
     */
    def |[U >: T](that: => Parser[U]): Parser[U] = Or(this, that)

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

  def success[T](t: T): Parser[T] = Parser { in => Success(t, in) }
  def opt[T](p: Parser[T]): Parser[Option[T]] = Parser { in => p(in) match {
    case Success(t, rest) => Success(Some(t), rest)
    case Failure(_, rest)    => Success(None, rest)
  }}

  /**
   * An ADT for parsers
   */
  case class FlatMapped[T, U](p: Parser[T], f: T => Parser[U]) extends Parser[U] {
    def apply(in: Input) = p(in) flatMapWithNext { res => rdr => f(res)(rdr) }
  }

  case class Mapped[T, U](p: Parser[T], f: T => U) extends Parser[U] {
    def apply(in: Input) = p(in) map f
  }

  case class Seq[T, U](l: Parser[T], r: Parser[U]) extends Parser[(T, U)] {
    lazy val bla = r
    /**
     * we just use the composition on parsers directly
     */
    lazy val inner = for (lRes <- l; rRes <- bla) yield (lRes, rRes)
    def apply(in: Input) = inner(in)
  }

  case class Or[T](l: Parser[T], r: Parser[T]) extends Parser[T] {
    def apply(in: Input) = l(in) orElse r(in)
  }

  /**
   * ParseResult, follows the implementation in the standard library
   */

  sealed abstract class ParseResult[+T] {

    def isSuccess = this match {
      case s @ Success(_, _) => true
      case _ => false
    }

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
