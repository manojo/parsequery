package parsec

import org.scalatest.FunSuite

/**
 * Testing HListParsers
 * The idea is that each parser lives as a separate tuple
 */
class HListParserSuite
    extends FunSuite
    with HListParsers
    with CharParsers {

  import shapeless._
  import ops.hlist._

  val myReader = CharReader("oh3hiagain!".toArray)

  test("parsing a single element gives a single result") {
    val pList = letter :: HNil

    val res: ParseResult[Char :: HNil] = parse(pList, myReader)

    res match {
      case Success(res, rest) => rest match {
        case CharReader(_, pos) => assert(res.head == 'o' && pos == 1)
        case _ => assert(false)
      }
      case _ => assert(false)
    }
  }

  test("parsing four elements works too") {
    val pList = letter :: letter :: digit :: letter :: HNil

    val res: ParseResult[Char :: Char :: Char :: Char :: HNil]
      = parse(pList, myReader)

    res match {
      case Success(res, rest) => rest match {
        case CharReader(_, pos) =>
          assert(res == 'o' :: 'h' :: '3' ::'h' :: HNil && pos == 4)
        case _ => assert(false)
      }
      case _ => assert(false)
    }
  }

}
