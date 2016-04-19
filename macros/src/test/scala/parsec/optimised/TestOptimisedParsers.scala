package parsec.optimised

import parsec._

import util.ParserFunSuite

/**
 * Tests functionality of the `optimise` macro
 */
class OptimisedParserSuite
    extends ParserFunSuite
    with OptimisedParsers {

  val myReader = CharReader("oh3hiagain!".toArray)

  val optimisedAccept: Parser[Char] = optimise(accept('o'))

  test("accept desugars into acceptIf") {
    checkSuccess(optimisedAccept, myReader)(expected = 'o', expectedPos = 1)
  }

}
