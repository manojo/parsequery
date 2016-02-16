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

  import util.Mappable._
  test("concating three repped parsers works") {

    val classicVowels = Set('a', 'e', 'i', 'o', 'u')
    def digit2Int: Parser[Int] = digit map { x => (x - '0').toInt }
    def isVowel(c: Char) = classicVowels contains c

    /**
     * The "parser" we write
     */
    val repConcats = rep(letter) :: rep(digit2Int) :: rep(letter) :: HNil

    /**
     * This is a bit ugly cause we see the `FoldParser` type
     * could we use some mad skills to give a better type?
     */
    def justTheStringPlease(fp: FoldParser[Char]): Parser[String] = {
      val (strZ, strCombine) = stringFolder
      fp.fold(strZ, strCombine).map(_.toString)
    }

    def addtheInts(fp: FoldParser[Int]) = fp.fold[Int](0, (acc, x) => acc + x)
    def upperCaseTheVowels(fp: FoldParser[Char]) = {
      val (strZ, strCombine) = stringFolder
      (fp filter isVowel map (_.toUpper)).fold(strZ, strCombine).map(_.toString)
    }

    /**
     * could we also possibly have a better syntax here?
     * I'm not sure what exactly that would be though.
     */
    val functions =
      (justTheStringPlease _) :: (addtheInts _) :: (upperCaseTheVowels _) :: HNil

    val mapped = map(repConcats, functions)
    val myInput = CharReader("oh12345idris!".toArray)

    val res: ParseResult[String :: Int :: String :: HNil] = parse(mapped, myInput)

    res match {
      case Success(res, rest) => rest match {
        case CharReader(_, pos) =>
          assert(res == "oh" :: 15 :: "II" :: HNil && pos == 12)
        case _ => assert(false)
      }
      case _ => assert(false)
    }

  }

}
