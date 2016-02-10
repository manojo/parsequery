package parsec

import scala.annotation.tailrec
import scala.collection.mutable.ListBuffer

import barbedwire._

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
    def flatMap[U](f: T => Parser[U]): Parser[U] = FlatMapped(this, f)

    def >>[U](f: T => Parser[U]) = flatMap(f)

    /**
     * The concat operation
     */
    def ~[U](that: => Parser[U]): Parser[(T, U)] = Seq(this, that)


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
    def | [U >: T](that: => Parser[U]): Parser[U] = Or(this, that)

  }

  /**
   * companion object for apply function
   */
  object Parser {
    def apply[T](f: Input => ParseResult[T]) = new Parser[T] {
      def apply(in: Input) = f(in)
    }
/*
    /**
     * A CPSList from a parser.
     */
    def fromParser[T, R](p: Parser[T]) = new CPSList[T, ParseResult[R]] {
      def fold(z: ParseResult[R], comb: Combine[T, ParseResult[R]]) = {

        @tailrec
        def loop(curRes: ParseResult[R]): ParseResult[R] = curRes match {
          //will always be a success here!
          case s @ Success(_, curIn) => p(curIn) match {
            /**
             * This feels a bit convoluted. We only want to combine
             * the underlying result and propagate the input.
             * But it makes sense that that is the responsibility
             * of the `comb` function after all
             */
            case s2 @ Success(res, rest) => loop(comb(s, s2))
            /**
             * The rest is where we started failing
             */
            case Failure(_, _) => s
          }
        }
        loop(z)
      }
    }
    */
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

  def rep[T, R](p: Parser[T])(implicit red: Reducer[T, R]): Parser[R] = Repeat(p)


  /**
   * An ADT for parsers
   */
  case class FlatMapped[T, U](p: Parser[T], f: T => Parser[U]) extends Parser[U] {
    def apply(in: Input) =  p(in) flatMapWithNext { res => rdr => f(res)(rdr) }
  }

  case class Mapped[T, U](p: Parser[T], f: T => U) extends Parser[U] {
    def apply(in: Input) = p(in) map f
  }

  case class Seq[T, U](l: Parser[T], r: Parser[U]) extends Parser[(T, U)] {

    /**
     * we just use the composition on parsers directly
     */
    val inner = for (lRes <- l; rRes <- r) yield (lRes, rRes)
    def apply(in: Input) = inner(in)
  }

  case class Or[T](l: Parser[T], r: Parser[T]) extends Parser[T] {
    def apply(in: Input) = l(in) orElse r(in)
  }

  /**
   * the repetition parser yields a `R` which is the result type
   * of a `CPSList`. This helps us compose list operations before
   * constructing the list.
   *
   * We need to have a CPSList hanging around, so we take it as an implicit
   * @TODO implicits so early seems like an overkill, will need to thinK
   * this through better.
   *
   * In fact we can't have CPSList. `rep` is not a Parser[CPSList], nor is it
   * a CPSList[Parser]. In fact it is both at the same time! IE a "transformed"
   * monad of both.
   * Either we make CPSList monadic (cumbersome), or we go back to
   * a proper `def repFold[T, R](p: Parser[T])(z: R, acc: (R, T) => R): Parser[R]`
   *
   * Let's make the (z, acc) parameter list implicit (so we don't need to specify),
   * give it a name (`Reducer`) and make an API for it (so we can map, filter etc. over it.)
   * Attention, `flatMap` will most prolly not work.
   *
   * Design of `Reducer` inspired by fastparse, cf. `Repeater`
   * https://github.com/lihaoyi/fastparse/blob/master/fastparse/shared/src/main/scala/fastparse/ParserApi.scala
   */
  case class Repeat[T, R](p: Parser[T])(implicit red: Reducer[T, R]) extends Parser[R] {
    def apply(in: Input) = {
      @tailrec
      def loop(curIn: Input, curRes: R): ParseResult[R] =
        p(curIn) match {
          case Success(res, rest) => loop(rest, red.combine(curRes, res))

          /**
           * The rest is where we started failing
           */
          case Failure(_, _) => Success(curRes, curIn)
        }

      loop(in, red.z)
    }
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

/**
 * A simple Reducer for now
 */
 case class Reducer[A, R](z: R, combine: (R, A) => R)
