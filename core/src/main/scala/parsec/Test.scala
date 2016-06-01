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

  object JsonParser {

    sealed abstract class JSValue
    case class JSObject(dict: List[(String, JSValue)]) extends JSValue
    case class JSArray(arr: List[JSValue]) extends JSValue
    case class JSDouble(d: Int) extends JSValue
    case class JSString(s: String) extends JSValue
    case class JSBool(b: Boolean) extends JSValue
    case object JSNull extends JSValue

    val jsonParser = optimise {

      def main: Parser[JSValue] = (
        //obj |
        arr |
        //stringLiteral.map(x => JSString(x)) |
        number.map(x => JSDouble(x)) |
        accept("null").map(_ => JSNull) |
        accept("true").map(_ => JSBool(true)) |
        accept("false").map(_ => JSBool(false))
      )

//      def obj: Parser[JSValue] = (
//        skipWs(accept('{')) ~> repsep(member, skipWs(accept(','))) <~ skipWs(accept('}'))
//      ) map { x => JSObject(x) }
//
      def arr: Parser[JSValue] = (
        skipWs(accept('[')) ~> repsep(main, skipWs(accept(','))) <~ skipWs(accept(']'))
      ) map { x => JSArray(x) }
//
//      def member: Parser[(String, JSValue)] =
//        stringLiteral ~ (skipWs(accept(':')) ~> main)

      main
    }
  }


  def main(args: Array[String]): Unit = {
    println("greetings lion")

    import scala.io.Source
    val fileName = "data/booleans-6600.json"
    val fileContent = Source.fromFile(fileName).mkString
    //val myReader = CharReader(fileContent.toArray)
    val myReader = CharReader("[ true, 123, [false, []] ]".toArray)

    val Success(res, rest) = JsonParser.jsonParser(myReader)
    println(res)
  }

}
