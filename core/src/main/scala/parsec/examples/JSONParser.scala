package parsec.examples

import parsec.optimised.OptimisedParsers
import parsec._

trait JSONParsers extends OptimisedParsers {

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
}

object JsonParser extends JSONParsers {
  def main(args: Array[String]): Unit = {
    println("greetings lion")

    import scala.io.Source
    val fileName = "data/booleans-6600.json"
    val fileContent = Source.fromFile(fileName).mkString
    val myReader = CharReader(fileContent.toArray)

    val Success(res, rest) = jsonParser(myReader)
    res match {
      case JSArray(ls) => println(ls.length)
      case _ => println("something else")
    }
  }
}
