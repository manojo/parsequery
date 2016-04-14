package util

import org.scalatest.FunSuite
import parsec._

/**
 * A util class for making it easier to write parser related tests
 */
class ParserFunSuite
    extends FunSuite
    with CharParsers {

  def checkSuccess[T](p: Parser[T], in: Input)
                     (expected: => T, expectedPos: => Int): Unit =
    checkSuccessH(p(in))(expected, expectedPos)

  def checkSuccessH[T](pr: ParseResult[T])
                      (expected: => T, expectedPos: => Int): Unit = {
    pr match {
      case Success(res, rest) => rest match {
        case CharReader(_, pos) => assert(res == expected && pos == expectedPos)
        case _ => assert(false, "Expecting a CharReader, but found some other Reader")
      }
      case f @ Failure(_, rest) => assert(
        false,
        s"Expecting a success, but parser failed on input at pos ${rest.asInstanceOf[CharReader].pos}"
      )
    }
  }
}
