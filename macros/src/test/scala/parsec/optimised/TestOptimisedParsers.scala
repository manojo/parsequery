package parsec.optimised

import _root_.util.ParserFunSuite
import parsec._

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
      def p = rep(letter)
      p
    }

    val numParser = optimise {
      def p = rep(digit)
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

  test("parsing strings works") {

    val greeting = "  greetings   lion"
    val greetReader = CharReader(greeting.toArray)

    checkFailure(optimise(accept("greetings")), CharReader("greetin".toArray))

    val greetingsLionParser = optimise {
      skipWs(accept("greetings")) ~ accept("lion")
    }

    checkSuccess(greetingsLionParser, greetReader)(
      expected = ("greetings", "lion"),
      expectedPos = greeting.length
    )
  }

  test("parsing string literals works") {

    val emptyStringLit = "\"\"".toArray
    val aQuote = "\"Dr. Livingstone, I presume?\"".toArray
    val quotesinQuotes =
      """"And then he said, \"Bwoy, you want something!\"."""".toArray

    val stringLitParser = optimise(stringLiteral)

    checkSuccess(stringLitParser, CharReader(emptyStringLit))(
      expected = "",
      expectedPos = emptyStringLit.length
    )

    checkSuccess(stringLitParser, CharReader(aQuote))(
      expected = "Dr. Livingstone, I presume?",
      expectedPos = aQuote.length
    )

    checkSuccess(stringLitParser, CharReader(quotesinQuotes))(
      expected = """And then he said, "Bwoy, you want something!".""",
      expectedPos = quotesinQuotes.length
    )

  }

  test("repsep works") {

    val noNames = "".toArray
    val justOneName = """ "Roger" """.toArray
    val names = """ "Roger", "Rafa", "Nole", "Stan" """.toArray

    val nameParser: Parser[List[String]] =
      optimise(repsep(skipWs(stringLiteral), accept(',')))

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

    val listOfAsParser = optimise {
      def listOfAs: Parser[List[Char]] = (
        (accept('a') ~ listOfAs).map { case (x, xs) => x :: xs } |
        success(Nil)
      )
      listOfAs
    }

    checkSuccess(listOfAsParser, CharReader("".toArray))(
      expected = List[Char](),
      expectedPos = 0
    )

    checkSuccess(listOfAsParser, CharReader("aaaaa".toArray))(
      expected = List('a','a','a','a','a'),
      expectedPos = 5
    )
  }

  test("double parsing works") {
    val doubleOpti = optimise(double)
    checkFailure(doubleOpti, CharReader("greetin".toArray))
    checkFailure(doubleOpti, CharReader("-.".toArray))
    checkSuccess(doubleOpti, CharReader("12345".toArray))(
      expected = 12345.0,
      expectedPos = "12345".length
    )

    checkSuccess(doubleOpti, CharReader("12.345".toArray))(
      expected = 12.345,
      expectedPos = "12.345".length
    )

    checkSuccess(doubleOpti, CharReader("-.12345e-6".toArray))(
      expected = -.12345e-6,
      expectedPos = "-.12345e-6".length
    )

    checkSuccess(doubleOpti, CharReader("-21312.12345e+125".toArray))(
      expected = -21312.12345e+125,
      expectedPos = "-21312.12345e+125".length
    )
  }

  test("opt parsing works") {
    val optOpti = optimise(opt(digit2Int))

    checkSuccess(optOpti, CharReader("2".toArray))(
      expected = Some(2),
      expectedPos = "2".length
    )

    checkSuccess(optOpti, CharReader("a".toArray))(
      expected = None,
      expectedPos = 0
    )

  }

  test("json parser works") {

    sealed abstract class JSValue
    case class JSObject(dict: List[(String, JSValue)]) extends JSValue
    case class JSArray(arr: List[JSValue]) extends JSValue
    case class JSDouble(d: Double) extends JSValue
    case class JSString(s: String) extends JSValue
    case class JSBool(b: Boolean) extends JSValue
    case object JSNull extends JSValue

    val jsonParser = optimise {

      def value: Parser[JSValue] = (
        obj |
        arr |
        stringLiteral.map(x => JSString(x)) |
        double.map(x => JSDouble(x)) |
        accept("null").map(_ => JSNull) |
        accept("true").map(_ => JSBool(true)) |
        accept("false").map(_ => JSBool(false))
      )

      def obj: Parser[JSValue] = (skipWs(accept('{')) ~>
        repsep(member, skipWs(accept(',')))
      <~ skipWs(accept('}'))) map { x => JSObject(x) }

      def arr: Parser[JSValue] = (skipWs(accept('[')) ~>
        repsep(value, skipWs(accept(',')))
      <~ skipWs(accept(']'))) map { x => JSArray(x) }

      def member: Parser[(String, JSValue)] =
        stringLiteral ~ (skipWs(accept(':')) ~> value)

      value
    }

    val bool = "true".toArray
    val strLit = "\"Roger pon top\"".toArray
    val num = "123".toArray
    val oneObj = """ {
      "name" : {
        "first": "Roger",
        "last": "Federer"
      },
      "magic_number": 1.2345456e+28,
      "quotable": "Je suis LE \"Roger\" que vous attendiez"
    }""".toArray

    checkSuccess(jsonParser, CharReader(bool))(
      expected = JSBool(true),
      expectedPos = bool.length
    )

    checkSuccess(jsonParser, CharReader(strLit))(
      expected = JSString("Roger pon top"),
      expectedPos = strLit.length
    )

    checkSuccess(jsonParser, CharReader(num))(
      expected = JSDouble(123),
      expectedPos = num.length
    )

    checkSuccess(jsonParser, CharReader(oneObj))(
      expected = JSObject(List(
        ("name", JSObject(
          List(
            ("first", JSString("Roger")),
            ("last", JSString("Federer"))
          )
        )),
        ("magic_number", JSDouble(1.2345456e+28)),
        ("quotable", JSString("""Je suis LE "Roger" que vous attendiez"""))
      )),
      expectedPos = oneObj.length
    )
  }

}
