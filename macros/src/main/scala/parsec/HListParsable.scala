package parsec

trait HListParsable {
  this: HListBoolean with Parsers with RepetitionParsers =>

  import shapeless._
  import ops.hlist._
  import syntax.singleton._

  /** Proof that parsing an HList of parsers yields a result */
  trait Parsable[L <: HList, B <: HList, Res <: HList] {
    def parse(ps: L, in: Input): ParseResult[Res]
  }

  /** Parsing a single parser with True gives a result (base case proof) */
  implicit def baseCaseParsableTrue[T] = {
    new Parsable[Parser[T] :: HNil, True :: HNil, T :: HNil] {
      def parse(ps: Parser[T] :: HNil, in: Input): ParseResult[T :: HNil] =
        ps.head(in).map { x => x :: HNil }
    }
  }

  /** Parsing a single parser with False gives a result (base case proof) */
  implicit def baseCaseParsableFalse[T] = {
    new Parsable[Parser[T] :: HNil, False :: HNil, HNil] {
      def parse(ps: Parser[T] :: HNil, in: Input): ParseResult[HNil] =
        ps.head(in).map { _ => HNil }
    }
  }

  /** Parsing multiple parsers with a True attached will ignore
   *  their result but still provide a result of the Parser. */
  implicit def recursiveParsableTrue[T, TS <: HList, BS <: HList, Res <: HList]
    (implicit parsable: Parsable[TS, BS, Res]) = {
      new Parsable[Parser[T] :: TS, True :: BS, T :: Res] {
        def parse(ps: Parser[T] :: TS, in: Input): ParseResult[T :: Res] = {
          ps.head(in) match {
            case f @ Failure(_, _) => f
            case Success(a, rest) => parsable.parse(ps.tail, rest) match {
              case f @ Failure(msg, _) => Failure(msg, in)
              case Success(as, rest2) => Success(a :: as, rest2)
            }
          }
        }
      }
    }

  /** Parsing multiple parsers with a False attached will ignore
   *  their result but still provide a result of the Parser. */
  implicit def recursiveParsableFalse[T, TS <: HList, BS <: HList, Res <: HList]
    (implicit parsable: Parsable[TS, BS, Res]) = {
      new Parsable[Parser[T] :: TS, False :: BS, Res] {
        def parse(ps: Parser[T] :: TS, in: Input): ParseResult[Res] = {
          ps.head(in) match {
            case f @ Failure(_, _) => f
            case Success(_, rest) => parsable.parse(ps.tail, rest) match {
              case f @ Failure(msg, _) => Failure(msg, in)
              case Success(as, rest2) => Success(as, rest2)
            }
          }
        }
      }
    }

  /** Parsing a HList of parsers gives us a HList of results */
  def parse[L <: HList, B <: HList, Res <: HList](xs: L, in: Input)
    (implicit parsable: Parsable[L, B, Res]): ParseResult[Res] =
      parsable.parse(xs, in)

  /** Lift an HList of parsers into a parser
   *
   * TODO Figure out why the parameter needs to be lazy */
  def mkParser[L <: HList, B <: HList, Res <: HList](xs: => L)
    (implicit parsable: Parsable[L, B, Res]): Parser[Res] =
      Parser { in => parsable.parse(xs, in) }
}

