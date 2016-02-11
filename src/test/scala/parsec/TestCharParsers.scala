package parsec

import org.scalatest.FunSuite

/**
 * Testing CharParsers
 */
class CharParsersSuite
    extends FunSuite
    with CharParsers {


  /**
   * Let's create a reducer for lists
   */
  implicit val listReducer: Reducer[Char, List[Char]] =
    Reducer(List[Char](), (ls, a) => ls :+ a)

  val myReader = CharReader("oh hai!".toArray)

  val twoCharParser = accept('o') ~ accept('h')

  /**
    * precising the return type forces the implicit search
    * to kick in for finding the `Reducer`
    */
  val wordParser: Parser[List[Char]] = word


  test("can parse two chars") {
    twoCharParser(myReader) match {
      case Success(res, rest) => rest match {
        case CharReader(_, pos) => assert(res == ('o', 'h') && pos == 2)
        case _ => assert(false)
      }

      case _ => assert(false)
    }
  }

  test("can parse a word") {
    wordParser(myReader) match {
      case Success(res, rest) => rest match {
        case CharReader(_, pos) => assert(res == List('o', 'h') && pos == 2)
        case _ => assert(false)
      }
      case _ => assert(false)
    }
  }
}

