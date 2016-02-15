package parsec

import org.scalatest.FunSuite

/**
 * Testing CharParsers
 */
class CharParsersSuite
    extends FunSuite
    with CharParsers {

  val myReader = CharReader("oh3hiagain!".toArray)

  test("can parse two chars") {
    val twoCharParser = accept('o') ~ accept('h')
    twoCharParser(myReader) match {
      case Success(res, rest) => rest match {
        case CharReader(_, pos) => assert(res == ('o', 'h') && pos == 2)
        case _ => assert(false)
      }

      case _ => assert(false)
    }
  }

  test("can parse a word") {
    val (strZ, strCombine) = stringFolder
    def wordParser: Parser[String] = letters.fold(strZ, strCombine).map(_.toString)

    wordParser(myReader) match {
      case Success(res, rest) => rest match {
        case CharReader(_, pos) => assert(res == "oh" && pos == 2)
        case _ => assert(false)
      }
      case _ => assert(false)
    }
  }

  test("can parse a word and a letter") {
    val (strZ, strCombine) = stringFolder

    def wordDigitLetter: Parser[(String, (Char, Char))]
      = (letters ~ digit ~ letter).fold(strZ, strCombine) map {
          case (ls, other) => (ls.toString, other)
        }

    wordDigitLetter(myReader) match {
      case Success(res, rest) => rest match {
        case CharReader(_, pos) =>
          assert(res == ("oh", ('3', 'h')) && pos == 4)
        case _ => assert(false)
      }
      case _ => assert(false)
    }
  }
}

