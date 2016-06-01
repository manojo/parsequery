package parsec.optimised.util

import scala.reflect.macros.blackbox.Context

trait Zeroval {

  val c: Context
  import c.universe._

  //scalastyle:off line.size.limit
  /**
   * Get the "zero" value of a certain type
   * @todo not make it work on strings
   * @param typ
   * Based on https://github.com/begeric/FastParsers/blob/experiment/FastParsers/src/main/scala/fastparsers/parsers/ParserImplBase.scala
   */
  //scalastyle:on line.size.limit

  def zeroValue(typ: Type): Tree = {
    if (typ =:= typeOf[scala.Char]) q"0"
    else if (typ =:= typeOf[scala.Int])  q"0"
    else if (typ =:= typeOf[scala.Unit]) q"()"
    else q"null"
  }
}
