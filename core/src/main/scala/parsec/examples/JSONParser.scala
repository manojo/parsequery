package parsec.examples

import parsec.optimised.OptimisedParsers

object JSONParser extends OptimisedParsers {

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

    def obj: Parser[JSValue] = (
      skipWs(accept('{')) ~> repsep(member, skipWs(accept(','))) <~ skipWs(accept('}'))
    ) map { x => JSObject(x) }

    def arr: Parser[JSValue] = (
      skipWs(accept('[')) ~> repsep(main, skipWs(accept(','))) <~ skipWs(accept(']'))
    ) map { x => JSArray(x) }

    def member: Parser[(String, JSValue)] =
      stringLiteral ~ (skipWs(accept(':')) ~> main)

    main
  }
}
