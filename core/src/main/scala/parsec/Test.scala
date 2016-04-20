package parsec

import parsec.optimised.OptimisedParsers

object Test extends OptimisedParsers {

  val optimisedParser = optimise {
    def p = accept('o') ~ accept('h')
    p
  }

  def main(args: Array[String]): Unit = {
    println("oh hai!")
    val myReader = CharReader("oh3hiagain!".toArray)
    println(optimisedParser(myReader))
  }
}
