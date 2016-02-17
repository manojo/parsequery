package parsec

/**
 * A trait for parsing characters
 */
trait CharParsers extends Parsers with RepetitionParsers {

  type Elem = Char

  /**
   * some other handy parsers
   */
  def letter: Parser[Char] = acceptIf(_.isLetter)
  def digit: Parser[Char] = acceptIf(_.isDigit)
  def digit2Int: Parser[Int] = digit map { c => (c - '0').toInt }
  def singleSpace: Parser[Char] = accept(' ')


  def letters = rep(letter)
  def digits = rep(digit)
  def number: Parser[Int] =
    rep(digit2Int).fold[Int](0, (acc, n) => acc * 10 + n)

  def ws = rep(singleSpace)
  def ignoreWs = ws.fold[Unit]((), (acc, _) => acc)

  /**
   * surrounds any parser with a whitespace ignoring parser
   * left and right
   */
  def skipWs[T](p: Parser[T]): Parser[T] =
    ignoreWs ~> p <~ ignoreWs

  /**
   * some handy reducers
   */
  import scala.collection.mutable.StringBuilder
  def stringFolder = (
    StringBuilder.newBuilder,
    (acc: StringBuilder, c: Char) => acc append c
  )
}

object HelloCharParsers extends CharParsers {

  val (strZ, strCombine) = stringFolder

  val wordDigitLetter: Parser[(String, Char)] = {
    (letters ~ digit).fold(strZ, strCombine) map {
      case (ls, other) => (ls.toString, other)
    }
  }

  def main(args: Array[String]): Unit = {
    val myReader = CharReader("oh3hiagain!".toArray)
    println(wordDigitLetter(myReader))
  }

}
