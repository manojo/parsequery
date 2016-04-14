package parsec

trait HListBoolean {

  import shapeless._
  import syntax.singleton._

  /* Define Boolean on the type level */
  val (wTrue, wFalse) = (true.witness, false.witness)
  type True = wTrue.T
  type False = wFalse.T

}

