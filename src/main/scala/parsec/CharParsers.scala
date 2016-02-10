package parsec

/**
 * A trait for parsing characters
 */
trait CharParsers extends Parsers {

  type Elem = Char

  /**
   * some other handy parsers
   */
  def letter: Parser[Char] = acceptIf(_.isLetter)

}

/**
 * Testing out how the parsers work y'all!
 */
object HelloCharParsers extends CharParsers {

  /**
   * Let's create a reducer for lists
   */
  implicit val listReducer: Reducer[Char, List[Char]] =
    Reducer(List[Char](), (ls, a) => ls :+ a)

  def main(args: Array[String]) {

    val input = "oh hai!"
    val aParser = accept('o') ~ accept('h')

    val anotherParser: Parser[List[Char]] = rep(letter)

    println("Hello, Parsers!")
    println(aParser(CharReader(input.toArray)))
    println(anotherParser(CharReader(input.toArray)))

  }
}
