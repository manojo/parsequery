package examples

import fastparse.all._

object HelloJSON {

  def main(args: Array[String]) {
    val parseA = P("a")

    val Parsed.Success(value, successIndex) = parseA.parse("a")
    assert(value == (), successIndex == 1)

    val failure = parseA.parse("b").asInstanceOf[Parsed.Failure]
    assert(failure.lastParser == ("a": P0))
    assert(failure.index == 0)
    assert(failure.extra.traced.trace == """parseA:1:1 / "a":1:1 ..."b"""")
  }
}
