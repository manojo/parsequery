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

  test("number parser works as expected") {

    val zero = CharReader("".toArray)
    val biggerNum = CharReader("12345".toArray)

    /** an empty string should not be a number */
    checkFailure(number, zero)
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

  test("parsing and recognizing strings works") {

    val greeting = "greetings lion"
    val greetReader = CharReader(greeting.toArray)

    checkFailure(accept("greetings"), CharReader("greetin".toArray))

    checkSuccess(skipWs(accept("greetings")) ~ accept("lion"), greetReader)(
      expected = ("greetings", "lion"),
      expectedPos = greeting.length
    )

    checkSuccess(
      skipWs(recognize("greetings")) ~> recognize("lion"), greetReader
    )(expected = (), expectedPos = greeting.length)
  }

  test("parsing string literals works") {

    val emptyStringLit = "\"\"".toArray
    val aQuote = "\"Dr. Livingstone, I presume?\"".toArray
    val quotesinQuotes =
      """"And then he said, \"Bwoy, you want something!\"."""".toArray

    checkSuccess(stringLiteral, CharReader(emptyStringLit))(
      expected = "",
      expectedPos = emptyStringLit.length
    )

    checkSuccess(stringLiteral, CharReader(aQuote))(
      expected = "Dr. Livingstone, I presume?",
      expectedPos = aQuote.length
    )

    checkSuccess(stringLiteral, CharReader(quotesinQuotes))(
      expected = """And then he said, "Bwoy, you want something!".""",
      expectedPos = quotesinQuotes.length
    )

  }

  test("repsep works") {

    val noNames = "".toArray
    val justOneName = """ "Roger" """.toArray
    val names = """ "Roger", "Rafa", "Nole", "Stan" """.toArray

    def nameParser: Parser[List[String]] = repsep(skipWs(stringLiteral), comma)

    checkSuccess(nameParser, CharReader(names))(
      expected = List("Roger", "Rafa", "Nole", "Stan"),
      expectedPos = names.length
    )

    checkSuccess(nameParser, CharReader(noNames))(
      expected = List(),
      expectedPos = noNames.length
    )

    checkSuccess(nameParser, CharReader(justOneName))(
      expected = List("Roger"),
      expectedPos = justOneName.length
    )

  }

  test("basic recursion works") {

    def listOfAs: Parser[List[Char]] = (
      (accept('a') ~ listOfAs).map { case (x, xs) => x :: xs } |
      success(Nil)
    )

    checkSuccess(listOfAs, CharReader("".toArray))(
      expected = List[Char](),
      expectedPos = 0
    )

    checkSuccess(listOfAs, CharReader("aaaaa".toArray))(
      expected = List('a','a','a','a','a'),
      expectedPos = 5
    )
  }
}
