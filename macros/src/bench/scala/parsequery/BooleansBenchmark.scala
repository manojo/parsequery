package parsequery

import util._

import org.scalameter.api._
import org.scalameter.picklers.Implicits._
import org.scalameter.Key

import parsec._
import parsec.optimised._

object BooleansData {
  val files = List("booleans-6600.json")

  implicit val range: Gen[List[String]] =
    Gen.enumeration("files")(files)
}

object BooleansBenchmark extends Bench.Group {

  performance of "foldToArrayBuffer" in {
    include(new BooleansArrayBufferBenchmark {})
  }

  performance of "optimisedBools" in {
    include(new BooleansOptimisedBenchmark {})
  }

  performance of "optimisedBoolsJSON" in {
    include(new BooleansJSONBenchmark {})
  }

  performance of "FastParseBools" in {
    include(new FastParseBooleansBenchmark {})
  }

  performance of "FastParseJSONBools" in {
    include(new FastParseJSONBooleansBenchmark {})
  }

  performance of "HandWrittenBools" in {
    include(new BooleanHandWrittenBenchmark {})
  }
}

trait BooleanHandWrittenBenchmark extends BenchmarkHelper {

  import BooleansData._

  final val TRUE = "true".toCharArray
  final val FALSE = "false".toCharArray

  import scala.collection.mutable.ArrayBuffer
  def parseBooleans(input: Array[Char]): ArrayBuffer[Array[Char]] = {

    import scala.annotation.tailrec

    @tailrec
    def whitespaces(i: Int): Int =
      if (i >= input.length) i
      else if (input(i) == ' ' || input(i) == '\n') whitespaces(i + 1)
      else i

    def parseChar(c: Char, i: Int): (Boolean, Int) = {
      val idxAfterSkipping = whitespaces(i)
      if (i >= input.length) (false, i)
      if (input(idxAfterSkipping) == c) (true, whitespaces(idxAfterSkipping + 1))
      else (false, i)
    }

    def comma(i: Int) = parseChar(',', i)
    def brackOpen(i: Int) = parseChar('[', i)
    def brackClose(i: Int) = parseChar(']', i)

    def parseLit(i: Int, arr: Array[Char]): (Boolean, Int) = {
      @tailrec
      def loop(tmpIdx: Int): Boolean = {
        if (tmpIdx == arr.length) true
        else if (input(i + tmpIdx) == arr(tmpIdx)) loop(tmpIdx + 1)
        else return false
      }
      if (loop(0)) (true, i + arr.length) else (false, i)
    }

    def parseBool(i: Int): (Boolean, Int, Option[Array[Char]]) = {
      val (trueParsed, idx1) = parseLit(i, TRUE)
      if (trueParsed) (true, idx1, Some(TRUE))
      else {
        val (falseParsed, idx2) = parseLit(i, FALSE)
        if (falseParsed) (true, idx2, Some(FALSE))
        else             (false, i, None)
      }
    }

    def repsepBools(i: Int): (Int, ArrayBuffer[Array[Char]]) = {
      // parses skipws(,) ~> (true | false)
      @tailrec
      def loop(tmpPos: Int, acc: ArrayBuffer[Array[Char]]): (Int, ArrayBuffer[Array[Char]]) = {
        val (commaParsed, idx1) = comma(tmpPos)
        if (commaParsed) {
          val (boolParsed, idx2, elem) = parseBool(idx1)
          if (boolParsed) {
            val Some(trueElem) = elem
            acc += trueElem
            loop(idx2, acc)
          } else (idx1, acc)
        } else (tmpPos, acc)
      }

      val res = ArrayBuffer.empty[Array[Char]]

      val (boolParsed, idx1, elem) = parseBool(i)
      if (boolParsed) {
        val Some(trueElem) = elem
        res += trueElem
        loop(idx1, res)
      } else (i, res)
    }

    val (brackOpened, idx1) = brackOpen(0)
    if (brackOpened) {
      val (idx2, res) = repsepBools(idx1)
      val (brackClosed, idx3) = brackClose(idx2)
      if (brackClosed) res else ???
    } else ???
  }

  def runBM(g: Gen[List[String]], mName: String, meth: Array[Char] => ArrayBuffer[Array[Char]]): Unit = {
    measure method mName in {
      using(g) in { fs =>
        for (f <- fs) {
          val fileName = "data/" + f
          val file = scala.io.Source.fromFile(fileName).mkString
          val fileArray = file.toCharArray
          performance of s"$mName benchmark" in {
            val res = meth(fileArray)
            //println(res.length)
          }
        }
      }
    }
  }

  performanceOfParsers { f: Gen[List[String]] =>
    runBM(f, "manyBools", parseBooleans _ )
  }(range)

}

/**
 * The most naive version of a boolean parser
 * it folds into an immutable list, so extremely inefficient
 */
trait BooleansVanillaBenchmark extends ParsequeryBenchmarkHelper {

  import BooleansData._

  def bool = accept("true") | accept("false")
  def manyBools = (accept('[') ~>
    repsepFold(bool, skipWs(comma)).toListF
  <~ accept(']'))

  performanceOfParsers { f: Gen[List[String]] =>
    runBM(f, "manyBools", manyBools)
  }(range)
}

/**
 * Folds booleans into a list buffer
 */
trait BooleansListBufferBenchmark extends ParsequeryBenchmarkHelper {

  import BooleansData._

  def bool = accept("true") | accept("false")
  def manyBools = (accept('[') ~>
    repsepFold(bool, skipWs(comma)).toListBufferF
  <~ accept(']'))

  performanceOfParsers { f: Gen[List[String]] =>
    runBM(f, "manyBools", manyBools)
  }(range)
}

/**
 * Folds booleans into an array buffer
 */
trait BooleansArrayBufferBenchmark extends ParsequeryBenchmarkHelper {

  import BooleansData._

  val bool = accept("true") | accept("false")
  val manyBools = (accept('[') ~>
    repsepFold(bool, skipWs(comma)).toArrayBufferF
  <~ accept(']'))

  performanceOfParsers { f: Gen[List[String]] =>
    runBM(f, "manyBools", manyBools)
  }(range)
}

/**
 * A recogniser for a list of booleans. Parses the booleans, though
 */
trait BooleansUnitBenchmark extends ParsequeryBenchmarkHelper {
  import BooleansData._

  def bool = accept("true") | accept("false")
  def manyBools = (accept('[') ~>
    repsepFold(bool, skipWs(comma)).toSkipper
  <~ accept(']'))

  performanceOfParsers { f: Gen[List[String]] =>
    runBM(f, "manyBools", manyBools)
  }(range)
}

/**
 * a recogniser for a list of booleans. Recognises the booleans
 */
trait BooleansRecoUnitBenchmark extends ParsequeryBenchmarkHelper {
  import BooleansData._

  def bool = recognize("true") | recognize("false")
  def manyBools = (accept('[') ~>
    repsepFold(bool, skipWs(comma)).toSkipper
  <~ accept(']'))

  performanceOfParsers { f: Gen[List[String]] =>
    runBM(f, "manyBools", manyBools)
  }(range)
}

/**
 * uses the `optimise` macro to partially evaluate the parser away
 */
trait BooleansOptimisedBenchmark extends ParsequeryBenchmarkHelper {
  import BooleansData._

  val manyBools = optimise {
    def p = (accept('[') ~>
      repsep((accept("true") | accept("false")), skipWs(accept(',')))
    <~ accept(']'))
    p
  }

  performanceOfParsers { f: Gen[List[String]] =>
    runBM(f, "manyBools", manyBools)
  }(range)
}

/**
 * uses the `optimise` macro to partially evaluate the parser away
 * uses a general json parser rather then a dedicated parser
 */
trait BooleansJSONBenchmark extends ParsequeryBenchmarkHelper {
  import BooleansData._

  sealed abstract class JSValue
  case class JSObject(dict: List[(String, JSValue)]) extends JSValue
  case class JSArray(arr: List[JSValue]) extends JSValue
  case class JSDouble(d: Int) extends JSValue
  case class JSString(s: String) extends JSValue
  case class JSBool(b: Boolean) extends JSValue
  case object JSNull extends JSValue

  val jsonParser = optimise {

    def main: Parser[JSValue] = (
      obj |
      arr |
      stringLiteral.map(x => JSString(x)) |
      number.map(x => JSDouble(x)) |
      accept("null").map(_ => JSNull) |
      accept("true").map(_ => JSBool(true)) |
      accept("false").map(_ => JSBool(false))
    )

    def obj: Parser[JSValue] = (skipWs(accept('{')) ~>
      repsep(member, skipWs(accept(',')))
    <~ skipWs(accept('}'))) map { x => JSObject(x) }

    def arr: Parser[JSValue] = (skipWs(accept('[')) ~>
      repsep(main, skipWs(accept(',')))
    <~ skipWs(accept(']'))) map { x => JSArray(x) }

    def member: Parser[(String, JSValue)] =
      stringLiteral ~ (skipWs(accept(':')) ~> main)

    main
  }

  performanceOfParsers { f: Gen[List[String]] =>
    runBM(f, "manyBools", jsonParser)
  }(range)
}

/**
 * a booleans list parser, written in `FastParse`
 * aka "the competition"
 */
trait FastParseBooleansBenchmark extends FastParseBenchmarkHelper {
  import BooleansData._
  import fastparse.all._

  case class NamedFunction[T, V](f: T => V, name: String) extends (T => V) {
    def apply(t: T): V = f(t)
    override def toString(): String = name
  }
  val Whitespace = NamedFunction(" \r\n".contains(_: Char), "Whitespace")
  val space = P(CharsWhile(Whitespace).?)

  val `false` = P("false")
  val `true` = P("true")
  val booleans = `true` | `false`

  val manyBools = P("[" ~/ booleans.rep(sep = ",".~/ ~ space) ~ space ~ "]")

  performanceOfParsers { f: Gen[List[String]] =>
    runBM(f, "FastParseBools", manyBools)
  }(range)
}


/**
 * a booleans list parser, written in `FastParse`. Also uses a JSON
 * parser instead of a dedicated bool parser
 */
trait FastParseJSONBooleansBenchmark extends FastParseBenchmarkHelper {
  import BooleansData._
  import fastparse.all._

  sealed abstract class JSValue
  case class JSObject(dict: List[(String, JSValue)]) extends JSValue
  case class JSArray(arr: List[JSValue]) extends JSValue
  case class JSDouble(d: Double) extends JSValue
  case class JSString(s: String) extends JSValue
  case class JSBool(b: Boolean) extends JSValue
  case object JSNull extends JSValue

  case class NamedFunction[T, V](f: T => V, name: String) extends (T => V) {
    def apply(t: T): V = f(t)
    override def toString(): String = name
  }
  val Whitespace = NamedFunction(" \r\n".contains(_: Char), "Whitespace")
  val Digits = NamedFunction('0' to '9' contains (_: Char), "Digits")
  val StringChars = NamedFunction(!"\"\\".contains(_: Char), "StringChars")

  val space = P(CharsWhile(Whitespace).?)
  val digits = P(CharsWhile(Digits))
  val exponent = P(CharIn("eE") ~ CharIn("+-").? ~ digits)
  val fractional = P("." ~ digits)
  val integral = P("0" | CharIn('1' to '9') ~ digits.?)

  val number = P(CharIn("+-").? ~ integral ~ fractional.? ~ exponent.?).!.map(
    x => JSDouble(x.toDouble)
  )

  val `null` = P("null").map(_ => JSNull)
  val `false` = P("false").map(_ => JSBool(false))
  val `true` = P("true").map(_ => JSBool(true))

  val hexDigit = P(CharIn('0' to '9', 'a' to 'f', 'A' to 'F'))
  val unicodeEscape = P("u" ~ hexDigit ~ hexDigit ~ hexDigit ~ hexDigit)
  val escape = P("\\" ~ (CharIn("\"/\\bfnrt") | unicodeEscape))

  val strChars = P(CharsWhile(StringChars))
  val string =
    P(space ~ "\"" ~/ (strChars | escape).rep.! ~ "\"").map(JSString)

  val array =
    P("[" ~/ jsonExpr.rep(sep = ",".~/) ~ space ~ "]").map(xs => JSArray(xs.toList))

  val pair: P[(String, JSValue)] = P(string.map(_.s) ~/ ":" ~/ jsonExpr)

  val obj =
    P("{" ~/ pair.rep(sep = ",".~/) ~ space ~ "}").map(xs => JSObject(xs.toList))

  val jsonExpr: P[JSValue] = P(
    space ~ (obj | array | string | `true` | `false` | `null` | number) ~ space
  )

  performanceOfParsers { f: Gen[List[String]] =>
    runBM(f, "FastParseBools", jsonExpr)
  }(range)
}
