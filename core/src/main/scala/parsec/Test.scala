package parsec

import parsec.optimised.OptimisedParsers

object TestFastParse {

  import fastparse.all._

  case class NamedFunction[T, V](f: T => V, name: String) extends (T => V) {
    def apply(t: T): V = f(t)
    override def toString(): String = name
  }
  val Whitespace = NamedFunction(" \r\n".contains(_: Char), "Whitespace")
  val space = P(CharsWhile(Whitespace).?)

  val `false` = P("false".!)
  val `true` = P("true".!)
  val booleans = `true` | `false`

  val manyBools = P("[" ~/ booleans.rep(sep = ",".~/ ~ space) ~ space ~ "]")

  def main(args: Array[String]): Unit = {
    println("oh hai!")

    import scala.io.Source
    val fileName = "data/booleans-6600.json"
    val fileContent = Source.fromFile(fileName).mkString

    val Parsed.Success(res, rest) = manyBools.parse(fileContent)
    println(res.length)
  }
}

object Test extends OptimisedParsers {

  val bool = accept("true".toArray) | accept("false".toArray)
  val skipped = skipWs(comma)

  import scala.collection.mutable.ArrayBuffer
  val manyBools = parseMany[ArrayBuffer[String]](List(
    (accept('['), true),
    (repsepFold(bool, skipped).toArrayBufferF, false),
    (accept(']'), true)
  ))

  val simpleParser = optimise {
    def p = accept("ooo") ~ rep(acceptIf(_ == 'o')) ~ accept('h')
    p
  }

  def main(args: Array[String]): Unit = {
    println("oh hai!")

    import scala.io.Source
    val fileName = "data/booleans-6600.json"
    val fileContent = Source.fromFile(fileName).mkString
    val myReader = CharReader("ooohhai".toArray)

    val Success(res, rest) = simpleParser(myReader)
    println(res)
  }

}
