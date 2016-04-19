package parsec

trait Reader[+T] {

  def first: T
  def rest: Reader[T]
  def atEnd: Boolean

}

trait StringReader[+T] extends Reader[T] {
  def source: Array[Char]
}

case class CharReader(val source: Array[Char], val pos: Int = 0)
    extends StringReader[Char] {

  def first = source(pos)
  def rest = CharReader(source, pos + 1)
  def atEnd = pos >= source.length

}
