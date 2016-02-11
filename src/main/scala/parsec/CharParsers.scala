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
  def word[R](implicit red: Reducer[Char, R]) = rep(letter)
  //def number[R](implicit red: Reducer[Char, R]) = rep(digit) map (_.toInt)

}
