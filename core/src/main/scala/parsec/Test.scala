package parsec

import parsec.optimised.OptimisedParsers

object Test extends OptimisedParsers {

  val optimisedAccept: Parser[Char] = optimise(accept('o'))

  def main(args: Array[String]) {
    println("oh hai!")
    val myReader = CharReader("oh3hiagain!".toArray)
    println(optimisedAccept(myReader))
  }
}
