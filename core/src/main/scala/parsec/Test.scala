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

  def main(args: Array[String]): Unit = {
    println("greetings lion")

    import scala.io.Source
    val fileName = "data/booleans-6600.json"
    val fileContent = Source.fromFile(fileName).mkString
    //val myReader = CharReader(fileContent.toArray)
    val myReader = CharReader("421hello people".toArray)

    val stringLitParser = optimise {
      opt(digit2Int) ~ digit2Int//.map { case (a, b) => a }
    }

    val Success(res, rest) = stringLitParser(myReader)
    println(res)
  }

}
