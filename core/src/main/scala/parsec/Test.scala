package parsec

import parsec.optimised.OptimisedParsers

object Test extends OptimisedParsers {

  val optimisedAccept: Parser[Char] = optimise {
    def p = acceptIf(_ == 'o')
    p
  }

  def main(args: Array[String]): Unit = {
    println("oh hai!")
    val myReader = CharReader("oh3hiagain!".toArray)
    println(optimisedAccept(myReader))
  }
}
