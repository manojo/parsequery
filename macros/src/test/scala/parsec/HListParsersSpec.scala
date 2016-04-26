package parsec

import scala.language.postfixOps
import util.ParserFunSuite

/** Spec for `HListParsef`
 *
 *  The idea is that each parser lives as a separate tuple */
class HListParserSpec
    extends ParserFunSuite
       with HListParsers
       with CharParsers {

  // Easier to type
  type P[T] = Parser[T]

  import shapeless._
  import nat._
  import ops.nat._
  import ops.hlist._

  val reader = CharReader("oh3hiagain!".toArray)

  test("parsing a single element gives a single result") {

    val parsers: Parser[Char] :: HNil = letter :: HNil
    // Force the type to compile
    val p: P[Char :: HNil] =
      project[P[Char] :: HNil, P[Char] :: HNil, _1 :: HNil, _1 :: HNil, Char :: HNil, Char :: HNil](parsers, _1 :: HNil)

    checkSuccessH(p(reader))(
      expected = 'o' :: HNil,
      expectedPos = 1
    )

  }

  /*

  test("parsing four elements works too") {

    type Parsers = P[Char] :: P[Char] :: P[Char] :: P[Char] :: HNil
    /* One less element here because of our projection! */
    val parsers = letter :: letter :: digit :: letter :: HNil
    // We still need to put reverse manually
    val p = project2(parsers.reverse, (_1 :: _3 :: _4 :: HNil).reverse)

    checkSuccessH(p(reader))(
      expected = 'o' :: '3' ::'h' :: HNil,
      expectedPos = 4
    )

  }


  import util.Mappable._

  test("concating three repped parsers works") {

    val vowels = Set('a', 'e', 'i', 'o', 'u')
    def isVowel(c: Char) = vowels contains c

    type Res = String :: Int :: String :: HNil
    type Projection = True :: True :: True :: HNil
    type Parsers = Parser[String] :: Parser[Int] :: Parser[String] :: HNil
    /* Represent `FoldParser` of each record in the parsed text */
    val repConcats = letters :: rep(digit2Int) :: letters :: HNil

    /* TODO Give a better type and don't expose the `FoldParser` */
    def justTheStringPlease(fp: FoldParser[Char]): Parser[String] =
      fp.toStringParser

    def addtheInts(fp: FoldParser[Int]): Parser[Int] =
      fp.fold[Int](0, (acc, x) => acc + x)

    def upperCaseTheVowels(fp: FoldParser[Char]): Parser[String] =
      (fp filter isVowel map (_.toUpper)).toStringParser

    /* TODO Have a better syntax here? */
    val fs = (justTheStringPlease _) :: (addtheInts _) ::
             (upperCaseTheVowels _)  :: HNil

    val parser: Parsers = map(repConcats, fs)
    val input = CharReader("oh12345idris!".toArray)
    val res: ParseResult[Res] =
      parse[Parsers, Projection, Res](parser, input)

    checkSuccessH(res)(
      expected = "oh" :: 15 :: "II" :: HNil,
      expectedPos = 12
    )

  }

  test("parse a simple csv file (all elements are known)") {

    import scala.collection.mutable.StringBuilder

    val comma = accept(',')
    val CRLF = accept('\n')

    // Def because we need a new `StringBuilder` each time
    def first: Parser[String] = letters.toStringParser
    def last: Parser[String] = letters.toStringParser
    def age: Parser[Int] = number

    type Person = String :: String :: Int :: HNil
    type Projection = True :: True :: True :: HNil
    type PersonParser = Parser[String] ::
      Parser[String] :: Parser[Int] :: HNil

    def personParser = mkParser[PersonParser, Projection, Person](
      skipWs(first <~ comma) :: skipWs(last <~ comma) ::
      skipWs(age <~ CRLF)    :: HNil
    )

    def peopleParser = rep(personParser)
    def adultsParserRes = peopleParser filter {
      case f :: l :: a :: HNil => a >= 18
    } toList

    val people = List(
      "Roger, Federer, 34",
      "Rafael, Nadal, 29",
      "Random, Kid, 16",
      "Novak, Djokovic, 28",
      "Random, KidB, 12"
    ).mkString("\n") + "\n"

    val res: List[Person] = List(
      "Roger" :: "Federer" :: 34 :: HNil,
      "Rafael" :: "Nadal" :: 29 :: HNil,
      "Novak" :: "Djokovic" :: 28 :: HNil
    )

    checkSuccess(adultsParserRes, CharReader(people.toArray))(
      expected = res,
      expectedPos = people.length
    )

  }
  */
}
