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

  test("simple blocks with more than a single expression") {
    val singleLetter: Parser[Char] = optimise {
      def p = acceptIf(_ == 'o')
      p
    }

    /**
     * should desugar p into `acceptIf`
     */
    val singleLetterAccept: Parser[Char] = optimise {
      def p = accept('o')
      p
    }
    checkSuccess(singleLetter, myReader)(expected = 'o', expectedPos = 1)
    checkSuccess(singleLetterAccept, myReader)(expected = 'o', expectedPos = 1)
  }

  test("concat of accept desugars") {
    val twoCharParser = optimise {
      def p = accept('o') ~ accept('h')
      p
    }

    val threeCharParser = optimise {
      def p = accept('o') ~ accept('h') ~ accept('3')
      p
    }

    val ignoreLeft = optimise {
      def p = accept('o') ~> accept('h')
      p
    }

    val ignoreRight = optimise {
      def p = accept('o') <~ accept('h')
      p
    }

    checkSuccess(twoCharParser, myReader)(
      expected = ('o', 'h'),
      expectedPos = 2
    )

    checkSuccess(threeCharParser, myReader)(
      expected = (('o', 'h'), '3'),
      expectedPos = 3
    )

    checkSuccess(ignoreLeft, myReader)(
      expected = ('h'),
      expectedPos = 2
    )

    checkSuccess(ignoreRight, myReader)(
      expected = ('o'),
      expectedPos = 2
    )
  }

  test("or of accept desugars") {
    val orParser = optimise {
      def p = accept('o') | accept('i')
      p
    }

    val orParser2 = optimise {
      def p = accept('i') | accept('o')
      p
    }

    checkSuccess(orParser, myReader)(
      expected = ('o'),
      expectedPos = 1
    )

    checkSuccess(orParser2, myReader)(
      expected = ('o'),
      expectedPos = 1
    )
  }

  test("rep comes back unscathed") {
    val wordParser = optimise {
      def p = rep(letter).map(_.toListF)
      p
    }

    val numParser = optimise {
      def p = rep(digit).map(_.toListF)
      p
    }

    checkSuccess(wordParser, myReader)(
      expected = (List('o', 'h')),
      expectedPos = 2
    )

    val numReader = CharReader("12345".toArray)
    checkSuccess(numParser, numReader)(
      expected = (List('1', '2', '3', '4', '5')),
      expectedPos = 5
    )
  }

}
