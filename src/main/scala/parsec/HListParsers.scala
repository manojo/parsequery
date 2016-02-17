package parsec

trait HListParsers extends Parsers with RepetitionParsers {
  import shapeless._
  import ops.hlist._


  /**
   * Proof that parsing an HList of parsers
   * yields a result of parsers
   */
  trait Parsable[L <: HList, Res <: HList] {
    def parse(ps: L, in: Input): ParseResult[Res]
  }

  /**
   * Parsing a single parser gives its result
   */
  implicit def singleParsable[T] = {
    new Parsable[Parser[T] :: HNil, T :: HNil] {
      def parse(ps: Parser[T] :: HNil, in: Input): ParseResult[T :: HNil]
        = ps.head(in).map { x => x :: HNil }
    }
  }

  /**
   * Parsing recursively works too
   */
  implicit def recParsable[T, TS <: HList, Res <: HList]
                          (implicit parsable: Parsable[TS, Res]) = {
    new Parsable[Parser[T] :: TS, T :: Res] {
      def parse(ps: Parser[T] :: TS, in: Input): ParseResult[T :: Res] = {
        val (hd, tl) = (ps.head, ps.tail)
        hd(in) match {
          case f @ Failure(_, _) => f
          case Success(a, rest) => parsable.parse(tl, rest) match {
            case f @ Failure(msg, _) => Failure(msg, in)
            case Success(as, rest2) => Success(a :: as, rest2)
          }
        }
      }
    }
  }

  /**
   * parsing a HList of parsers,
   * gives us a HList of results
   */
  def parse[L <: HList, Res <: HList]
           (xs: L, in: Input)
           (implicit parsable: Parsable[L, Res]): ParseResult[Res] =
    parsable.parse(xs, in)
}
