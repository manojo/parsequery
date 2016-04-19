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

  test("acceptIf comes back unscathed") {
    val singleLetter: Parser[Char] = optimise(acceptIf(_ == 'o'))
    checkSuccess(singleLetter, myReader)(expected = 'o', expectedPos = 1)
  }

  test("accept desugars into acceptIf") {
    val singleLetter: Parser[Char] = optimise(accept('o'))
    checkSuccess(singleLetter, myReader)(expected = 'o', expectedPos = 1)
  }

}
