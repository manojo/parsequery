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
    def justTheStringPlease(fp: FoldParser[Char]): Parser[String] =
      fp.toStringParser

    def addtheInts(fp: FoldParser[Int]) = fp.fold[Int](0, (acc, x) => acc + x)
    def upperCaseTheVowels(fp: FoldParser[Char]) =
      (fp filter isVowel map (_.toUpper)).toStringParser

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
    def first = letters.toStringParser
    def last = letters.toStringParser
    def age = number

    type Person = String :: String :: Int :: HNil

    def personRecord = mkParser(
      skipWs(first <~ comma) :: skipWs(last <~ comma) :: skipWs(age <~ CRLF) :: HNil
    )

    def peopleParser = rep(personRecord)
    def adultsParser = peopleParser filter {
      case f :: l :: a :: HNil => a >= 18
    }

    def adultList = adultsParser.toListParser

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

  /* Let's try mimic the structure of the contributions file */

  /* parses the total key value pair */
  def total: Parser[Int] = (skipWs(accept('\"') ~> recognize("total") ~> accept('\"')) ~>
    accept(':') ~> skipWs(number))

  /* parses a week. See format below for what it looks like */
  def weekParser: Parser[List[(String, Int)]] = (skipWs(accept('{')) ~> CRLF ~>
    repsep(skipWs((stringLiteral <~ accept(':'))) ~ skipWs(number), skipWs(comma <~ CRLF)).toListParser
  <~ CRLF <~ skipWs(accept('}')))

  /* parses multiple weeks */
  def weeks: Parser[List[List[(String, Int)]]] = (skipWs(accept('\"') ~> recognize("weeks") ~> accept('\"')) ~>
    (accept(':') ~> skipWs(accept('[')) ~> CRLF) ~>
    repsep(weekParser, skipWs(comma <~ CRLF)).toListParser
  <~ CRLF <~ skipWs(accept(']')))

  /* parses author key-value pair */
  def authorName: Parser[String] = (skipWs(accept('\"') ~> recognize("author") ~> accept('\"'))
    ~> accept(':') ~> authorInnerInfo
  )

  /* parses login key-value pair */
  def login: Parser[String] = (skipWs(accept('\"') ~> recognize("login") ~> accept('\"'))
    ~> accept(':') ~> skipWs(stringLiteral)
  )

  /* parses author inner info */
  def authorInnerInfo: Parser[String] = (skipWs(accept('{')) ~> CRLF ~>
    (login <~ skipWs(comma <~ CRLF)) <~
    repsep(skipWs((stringLiteral <~ accept(':'))) ~ skipWs(stringLiteral), skipWs(comma <~ CRLF)).toSkipper
  <~ CRLF <~ skipWs(accept('}')))

  def oneContribution = (
    skipWs(accept('{')) ~> CRLF ~> (total <~ skipWs(comma <~ CRLF)) ~
    (weeks <~ skipWs(comma <~ CRLF)) ~
    authorName <~ CRLF <~ skipWs(accept('}'))
  )



  test("github contributions parser: can parse a single week") {

    /**
     * @TODO, this parser is a bit brittle. It should be able to hand spaces and
     * line breaks as the same thing to work properly
     */
    val noWeekInfo = """{

    }""".toArray

    val oneWeekInfo ="""{
      "w": 1333238400,
      "a": 0,
      "d": 0,
      "c": 0
    }""".toArray

    checkSuccess(weekParser, CharReader(noWeekInfo))(
      expected = List(),
      expectedPos = noWeekInfo.length
    )

    checkSuccess(weekParser, CharReader(oneWeekInfo))(
      expected = List(("w", 1333238400), ("a", 0), ("d", 0), ("c", 0)),
      expectedPos = oneWeekInfo.length
    )
  }

  test("github contributions parser: can parse multiple weeks") {

    /**
     * @TODO, this parser is a bit brittle. It should be able to hand spaces and
     * line breaks as the same thing to work properly
     */
    val noWeeks = """"weeks": [

    ]""".toArray

    val oneWeek =""""weeks": [
      {
        "w": 1333238400,
        "a": 0,
        "d": 0,
        "c": 0
      }
    ]""".toArray

    val threeWeeks =""""weeks": [
      {
        "w": 1333238400,
        "a": 0,
        "d": 0,
        "c": 0
      },
      {
        "w": 1333843200,
        "a": 0,
        "d": 0,
        "c": 0
      },
      {
        "w": 1334448000,
        "a": 0,
        "d": 0,
        "c": 0
      }
    ]""".toArray

    checkSuccess(weeks, CharReader(noWeeks))(
      expected = List(),
      expectedPos = noWeeks.length
    )

    checkSuccess(weeks, CharReader(oneWeek))(
      expected = List(
        List(("w", 1333238400), ("a", 0), ("d", 0), ("c", 0))
      ),
      expectedPos = oneWeek.length
    )

    checkSuccess(weeks, CharReader(threeWeeks))(
      expected = List(
        List(("w", 1333238400), ("a", 0), ("d", 0), ("c", 0)),
        List(("w", 1333843200), ("a", 0), ("d", 0), ("c", 0)),
        List(("w", 1334448000), ("a", 0), ("d", 0), ("c", 0))
      ),
      expectedPos = threeWeeks.length
    )
  }

  test("can parse login info") {
    val loginInfo = """ "login": "dotta" """.toArray
    checkSuccess(login, CharReader(loginInfo))(
      expected = "dotta",
      expectedPos = loginInfo.length
    )
  }

  test("github contributions parser: can parse a single contribution") {
    val singleContribution = """{
      "total": 1,
      "weeks": [
        {
          "w": 1333238400,
          "a": 0,
          "d": 0,
          "c": 0
        },
        {
          "w": 1333843200,
          "a": 0,
          "d": 0,
          "c": 0
        },
        {
          "w": 1334448000,
          "a": 0,
          "d": 0,
          "c": 0
        }
      ],
      "author": {
        "login": "dotta",
        "id": "703748",
        "avatar_url": "https://avatars.githubusercontent.com/u/703748?v=3",
        "gravatar_id": "",
        "url": "https://api.github.com/users/dotta",
        "html_url": "https://github.com/dotta",
        "followers_url": "https://api.github.com/users/dotta/followers",
        "following_url": "https://api.github.com/users/dotta/following{/other_user}",
        "gists_url": "https://api.github.com/users/dotta/gists{/gist_id}",
        "starred_url": "https://api.github.com/users/dotta/starred{/owner}{/repo}",
        "subscriptions_url": "https://api.github.com/users/dotta/subscriptions",
        "organizations_url": "https://api.github.com/users/dotta/orgs",
        "repos_url": "https://api.github.com/users/dotta/repos",
        "events_url": "https://api.github.com/users/dotta/events{/privacy}",
        "received_events_url": "https://api.github.com/users/dotta/received_events",
        "type": "User",
        "site_admin": "false"
      }
    }""".toArray

    checkSuccess(oneContribution, CharReader(singleContribution))(
      expected = (
        (1,
         List(
          List(("w", 1333238400), ("a", 0), ("d", 0), ("c", 0)),
          List(("w", 1333843200), ("a", 0), ("d", 0), ("c", 0)),
          List(("w", 1334448000), ("a", 0), ("d", 0), ("c", 0))
         )
        ), "dotta"),
      expectedPos = singleContribution.length
    )
  }
}
