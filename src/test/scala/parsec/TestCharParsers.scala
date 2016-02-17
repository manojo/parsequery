package parsec

import util.ParserFunSuite

/**
 * Testing CharParsers
 */
class CharParsersSuite
    extends ParserFunSuite
    with CharParsers {

  val myReader = CharReader("oh3hiagain!".toArray)

  import scala.collection.mutable.StringBuilder
  def stringFolder = (
    StringBuilder.newBuilder,
    (acc: StringBuilder, c: Char) => acc append c
  )

  test("can parse two chars") {
    val twoCharParser = accept('o') ~ accept('h')
    checkSuccess(twoCharParser, myReader)(
      expected = ('o', 'h'),
      expectedPos = 2
    )
  }

  test("can parse a word") {
    val wordParser: Parser[String] = letters.toStringParser
    checkSuccess(wordParser, myReader)(expected = "oh", expectedPos = 2)
  }

  test("can parse a word, a digit, and another letter") {
    val (strZ, strCombine) = stringFolder

    val wordDigitLetter: Parser[(String, (Char, Char))] = {
      (letters ~ digit ~ letter).fold(strZ, strCombine) map {
        case (ls, other) => (ls.toString, other)
      }
    }

    checkSuccess(wordDigitLetter, myReader)(
      expected = ("oh", ('3', 'h')), expectedPos = 4
    )
  }

  test("can parse a digit, then a word") {
    val (strZ, strCombine) = stringFolder

    val digitword: Parser[(Char, String)] =
      (digit ~ letters.fold(strZ, strCombine)) map {
        case (other, ls) => (other, ls.toString)
      }

    checkSuccess(digitword, CharReader("3hiagain!".toArray))(
      expected = ('3', "hiagain"), expectedPos = 8
    )
  }

  test("can parse a word, then some letters") {

    val wordsdigits: Parser[(String, String)] = {
      (letters ~ digits).fold(stringFolder, stringFolder) map {
        case (ls, ds) => (ls.toString, ds.toString)
      }
    }

    checkSuccess(wordsdigits, CharReader("hithere12345!".toArray))(
      expected = ("hithere", "12345"), expectedPos = 12
    )
  }

  test("number parser works as expected") {

    val zero = CharReader("".toArray)
    val biggerNum = CharReader("12345".toArray)

    checkSuccess(number, zero)(expected = 0, expectedPos = 0)
    checkSuccess(number, biggerNum)(expected = 12345, expectedPos = 5)
  }

  test("whitespace parser skips whitespaces") {

    val spacedInput = "   o           h"
    val spacedReader = CharReader(spacedInput.toArray)

    val spacedLetters: Parser[(Char, Char)] =
      (ignoreWs ~> accept('o')) ~ (ignoreWs ~> accept('h'))

    checkSuccess(spacedLetters, spacedReader)(
      expected = ('o', 'h'), expectedPos = spacedInput.length
    )

  }

  test("the ignore parsers propagate position") {
    checkSuccess(accept('o') ~> accept('h'), myReader)(
      expected = ('h'),
      expectedPos = 2
    )

    checkSuccess(accept('o') <~ accept('h'), myReader)(
      expected = ('o'),
      expectedPos = 2
    )
  }

}
