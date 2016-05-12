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

  //performance of "booleansANF" in {
  //  include(new BooleansANFBenchmark {})
  //}

  //performance of "booleansArray" in {
  //  include(new BooleansArrayBenchmark {})
  //}

  //performance of "FastParseBools" in {
  //  include(new FastParseBooleansBenchmark {})
  //}

  //performance of "HandWrittenBools" in {
  //  include(new BooleanHandWrittenBenchmark {})
  //}
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

trait BooleansArrayBenchmark extends ParsequeryBenchmarkHelper {
  import BooleansData._

  final val TRUE = "true".toArray
  final val FALSE = "false".toArray

  def bool = accept(TRUE) | accept(FALSE)
  def manyBools = (accept('[') ~>
    repsepFold(bool, skipWs(comma)).toArrayBufferF
  <~ accept(']'))

  performanceOfParsers { f: Gen[List[String]] =>
    runBM(f, "manyBools", manyBools)
  }(range)
}

trait BooleansANFBenchmark extends ParsequeryBenchmarkHelper {
  import BooleansData._

  final val TRUE = "true"; final val FALSE = "false"

  val tr = accept(TRUE)
  val fl = accept(FALSE)
  val bool = tr | fl

  val brackOpen = accept('[')
  val c = accept(',')
  lazy val skipped = skipWs(c)
  val brackClosed = accept(']')
  lazy val folded = repsepFold(bool, skipped).toArrayBufferF

  def manyBools = (brackOpen ~> folded <~ brackClosed)

  performanceOfParsers { f: Gen[List[String]] =>
    runBM(f, "manyBools", manyBools)
  }(range)
}

trait BooleansParseManyBenchmark extends ParsequeryBenchmarkHelper {
  import BooleansData._

  def bool = accept("true") | accept("false")
  lazy val brackOpen = accept('[')
  lazy val brackClosed = accept(']')
  def bools = repsepFold(bool, skipWs(comma)).toArrayBufferF

  def manyBools = parseMany[List[String]](List(
    (brackOpen, true),
    (bools, false),
    (brackClosed, true)
  ))

  performanceOfParsers { f: Gen[List[String]] =>
    runBM(f, "manyBools", manyBools)
  }(range)
}

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
