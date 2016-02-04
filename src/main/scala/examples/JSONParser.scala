package examples

import fastparse.all._

object HelloJSON extends JSONParser {

  import Js._

  def main(args: Array[String]) {

    val src: String = io.Source.fromFile("data/scala-lang-contributions.json").mkString

    /**
     * We are interested in the (author.id, total) pairs
     * in our contributions data.
     * Either we parse the whole thing, and then filter data:
     */
    val Parsed.Success(resAll, _) = jsonExpr.parse(src)


    val ids2totals: List[(Val, Val)] = (resAll match {
      case x @ Arr(ls) =>
        for(l <- ls) yield (l("author")("id"), l("total"))
    }).toList

    println(ids2totals.size)

    /**
     * Or we run a specialized parser
     */
    val parsed = projections.parse(src)

    val Parsed.Success(specialized, _) = parsed
    val ids2totalsBis: List[Val] = (specialized match {
      case x @ Arr(ls) => ls
    }).toList

    println(ids2totalsBis.size)

  }
}

/*
 * A very small, very simple JSON AST
 * Has to be declaired outside a trait for
 * value classes to work
 */
object Js {
  sealed trait Val extends Any {
    def value: Any
    def apply(i: Int): Val = this.asInstanceOf[Arr].value(i)
    def apply(s: java.lang.String): Val =
      this.asInstanceOf[Obj].value.find(_._1 == s).get._2
  }

  case class Unitt(value: Char) extends AnyVal with Val
  case class Str(value: java.lang.String) extends AnyVal with Val
  case class Obj(value: Map[java.lang.String, Val]) extends AnyVal with Val
  case class Arr(value: Array[Val]) extends AnyVal with Val
  case class Num(value: Double) extends AnyVal with Val

  case object False extends Val {
    def value = false
  }
  case object True extends Val {
    def value = true
  }
  case object Null extends Val {
    def value = null
  }
}

/**
 * grabbing the JSON Parser from fastparse
 * @see https://github.com/lihaoyi/fastparse/blob/master/fastparse/shared/src/test/scala/fastparse/JsonTests.scala
 */
trait JSONParser {

   case class NamedFunction[T, V](f: T => V, name: String) extends (T => V){
     def apply(t: T) = f(t)
     override def toString() = name

   }
   // Here is the parser
   val Whitespace = NamedFunction(" \r\n".contains(_: Char), "Whitespace")
   val Digits = NamedFunction('0' to '9' contains (_: Char), "Digits")
   val StringChars = NamedFunction(!"\"\\".contains(_: Char), "StringChars")

   val space         = P( CharsWhile(Whitespace).? )
   val digits        = P( CharsWhile(Digits))
   val exponent      = P( CharIn("eE") ~ CharIn("+-").? ~ digits )
   val fractional    = P( "." ~ digits )
   val integral      = P( "0" | CharIn('1' to '9') ~ digits.? )

   val number = P( CharIn("+-").? ~ integral ~ fractional.? ~ exponent.? ).!.map(
     x => Js.Num(x.toDouble)
   )

   val `null`        = P( "null" ).map(_ => Js.Null)
   val `false`       = P( "false" ).map(_ => Js.False)
   val `true`        = P( "true" ).map(_ => Js.True)

   val hexDigit      = P( CharIn('0'to'9', 'a'to'f', 'A'to'F') )
   val unicodeEscape = P( "u" ~ hexDigit ~ hexDigit ~ hexDigit ~ hexDigit )
   val escape        = P( "\\" ~ (CharIn("\"/\\bfnrt") | unicodeEscape) )

   val strChars = P( CharsWhile(StringChars) )
   val string =
     P( space ~ "\"" ~/ (strChars | escape).rep.! ~ "\"").map(Js.Str)

   val array =
     P( "[" ~/ jsonExpr.rep(sep=",".~/) ~ space ~ "]").map( xs => Js.Arr(xs.toArray))

   val pair = P( string.map(_.value) ~/ ":" ~/ jsonExpr )

   val obj =
     P( "{" ~/ pair.rep(sep=",".~/) ~ space ~ "}").map( xs => Js.Obj(xs.toMap) )

   val jsonExpr: P[Js.Val] = P(
     space ~ (obj | array | string | `true` | `false` | `null` | number) ~ space
   )

   //val total = P( "total" ~/ ":" ~/ jsonExpr ).map(e => Js.Str("total") -> e)
   val total: Parser[Js.Num] = P( "\"total\"" ~ ":" ~ space ~ number ~ space ~ "," ~ space)

   val notRightBracket = NamedFunction(!"]".contains(_: Char), "NotRightBracket")
   val anyCharNotRightBracket = P( CharsWhile(notRightBracket) )
   val notRightCurlyBracket = NamedFunction(!"}".contains(_: Char), "NotRightCurlyBracket")
   val untilRightCurlyBracket = P( CharsWhile(notRightCurlyBracket) )
   val untilCommaFunction = NamedFunction(!",".contains(_: Char), "UntilComma")
   val untilComma = P( CharsWhile(untilCommaFunction) )

   val logged = scala.collection.mutable.Buffer.empty[String]
   implicit val logger = fastparse.Logger(logged.append(_))
   val stringPair = P( string ~ ":" ~ space ~ string ~ space ~ "," )

   val unitToken = Js.Unitt('a')
   val weeks: Parser[Js.Unitt] = P( "\"weeks\"" ~ space ~ ":" ~ space ~
     "[" ~ space ~ anyCharNotRightBracket ~ space ~ "]," ~ space).map(_ => unitToken)
   val author: Parser[Js.Str] = P("\"author\"" ~ space ~ ":" ~ space ~
     "{" ~ space ~ "\"login\"" ~ space ~ ":" ~ space ~ string ~ "," ~
     untilComma.rep(sep=",",max=15) ~ space ~ untilRightCurlyBracket ~ space ~  "}" ~ space)

   val projection: P[Js.Arr] =
     P( space ~ "{" ~ space ~ total ~ weeks ~ author ~ space ~ "}" ~ space).map {
       case (t,w,a) => Js.Arr(Array(t, a))
     }

   val projections: P[Js.Arr] =
     P( "[" ~/ projection.rep(sep=",".~/) ~ "]").map(xs => Js.Arr(xs.toArray))

   /*val schema =
     P( "[" ~/ jsonExpr.rep(sep=",".~/) ~ space ~ "]").map(Js.Arr(_:_*))

   val specializedParser: P[Js.Val] = P(
     space ~ (schema) ~ space
   )*/
}
