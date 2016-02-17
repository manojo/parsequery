package parsec

import util.ParserFunSuite

/**
 * Testing HListParsers
 * The idea is that each parser lives as a separate tuple
 */
class HListParserSuite
    extends ParserFunSuite
    with HListParsers
    with CharParsers {

  import shapeless._
  import ops.hlist._

  val myReader = CharReader("oh3hiagain!".toArray)

  test("parsing a single element gives a single result") {
    val pList = letter :: HNil
    val res: ParseResult[Char :: HNil] = parse(pList, myReader)

    checkSuccessH(res)(expected = 'o' :: HNil, expectedPos = 1)
  }

  test("parsing four elements works too") {
    val pList = letter :: letter :: digit :: letter :: HNil

    val res: ParseResult[Char :: Char :: Char :: Char :: HNil]
      = parse(pList, myReader)

    checkSuccessH(res)(
      expected = 'o' :: 'h' :: '3' ::'h' :: HNil, expectedPos = 4
    )

  }

  import util.Mappable._
  test("concating three repped parsers works") {

    val classicVowels = Set('a', 'e', 'i', 'o', 'u')
    def isVowel(c: Char) = classicVowels contains c

    /**
     * The "parser" we write
     */
    val repConcats = letters :: rep(digit2Int) :: letters :: HNil

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

    val res: ParseResult[String :: Int :: String :: HNil] =
      parse(mapped, myInput)

    checkSuccessH(res)(
      expected =  "oh" :: 15 :: "II" :: HNil, expectedPos = 12
    )
  }

  test("a simple csv like parser, all elements known") {

    import scala.collection.mutable.StringBuilder

    /**
     * Has to be a def since we need to always have a new
     * stringbuilder
     */
    def first = letters.fold(
      StringBuilder.newBuilder,
      (acc: StringBuilder, c: Char) => acc append c
    ).map(_.toString)

    def last = letters.fold(
      StringBuilder.newBuilder,
      (acc: StringBuilder, c: Char) => acc append c
    ).map(_.toString)

    def age = number

    val comma = accept(',')
    val CRLF = accept('\n')

    type Person = String :: String :: Int :: HNil

    def personRecord = mkParser(
      skipWs(first <~ comma) :: skipWs(last <~ comma) :: skipWs(age <~ CRLF) :: HNil
    )

    def peopleParser = rep(personRecord)
    def adultsParser = peopleParser filter { case f :: l :: a :: HNil => a >= 18 }

    def adultList = adultsParser.fold(
      List[Person](),
      (acc: List[Person], p: Person) => acc :+ p
    )

    val people = List(
      "Roger, Federer, 34",
      "Rafael, Nadal, 29",
      "Random, Kid, 16",
      "Novak, Djokovic, 28",
      "Random, KidB, 12"
    ).mkString("\n") + "\n"

    val res = List(
      "Roger" :: "Federer" :: 34 :: HNil,
      "Rafael" :: "Nadal" :: 29 :: HNil,
      "Novak" :: "Djokovic" :: 28 :: HNil
    )

    checkSuccess(adultList, CharReader(people.toArray))(
      expected = res,
      expectedPos = people.length
    )
  }

}
