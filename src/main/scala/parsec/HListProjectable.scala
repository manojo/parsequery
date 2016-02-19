package parsec

trait HListProjectable {
  this: HListBoolean with Parsers with RepetitionParsers =>

  import shapeless._
  import nat._
  import ops.nat._
  import ops.hlist._
  import syntax.singleton._

  /**
   * Proof that parsing an HList of parsers yields a result
   */
  trait Projectable[PS <: HList, NS <: HList, Proj <: HList] {
    def project(ps: PS, ns: NS): Parser[Proj]
  }

  /**
   * Gives an empty parser if there is no number left
   */
  implicit def baseCaseProjectableNeq = {
    new Projectable[Parser[_] :: HNil, HNil, HNil] {
      def project(ps: Parser[_] :: HNil, ns: HNil): Parser[HNil] = {
        Parser { in =>
          ps.head(in).map { _ => HNil }
        }
      }
    }
  }

  /**
   * Gives a parser with the correct projection if pos and length match
   */
  implicit def baseCaseProjectableEq[T] = {
    new Projectable[Parser[T] :: HNil, _1 :: HNil, T :: HNil] {
      def project(ps: Parser[T] :: HNil, ns: _1 :: HNil): Parser[T :: HNil] = {
        Parser { in =>
          ps.head(in).map { r => r :: HNil }
        }
      }
    }
  }

  /*
   implicit def baseCaseProjectable[NS <: HList] = {
    new Projectable[HNil, NS, HNil] {
      def project(ps: HNil, ns: NS): Parser[HNil] =
        Parser { in => Success(HNil,in) }
    }
  }
  */

  implicit def recursiveProjectableEq[T, PS <: HList, N <: Nat, NS <: HList, Proj <: HList]
    (implicit len: Length.Aux[Parser[T] :: PS, N],
      projectable: Projectable[PS, NS, Proj]) = {
      new Projectable[Parser[T] :: PS, N :: NS, T :: Proj] {
        def project(ps: Parser[T] :: PS, ns: N :: NS): Parser[T :: Proj] = {
          Parser { in =>
            projectable.project(ps.tail, ns.tail)(in) match {
              case f: Failure => f
              case Success(r1, in2) =>
                ps.head(in2) match {
                  case f @ Failure(msg, _) => Failure(msg, in)
                  case Success(r2, rest2) => Success(r2 :: r1, rest2)
                }
            }
          }
        }
      }
    }

  implicit def recursiveProjectableLt[T, PS <: HList, N <: Nat, PSL <: Nat, NS <: HList, Proj <: HList]
    (implicit len: Length.Aux[Parser[T] :: PS, PSL], lt: LT[N, PSL],
      projectable: Projectable[PS, N :: NS, Proj]) = {
      new Projectable[Parser[T] :: PS, N :: NS, Proj] {
        def project(ps: Parser[T] :: PS, ns: N :: NS): Parser[Proj] = {
          Parser { in =>
            projectable.project(ps.tail, ns)(in) match {
              case f: Failure => f
              case Success(r1, in2) =>
                ps.head(in2) match {
                  case f @ Failure(msg, _) => Failure(msg, in)
                  case Success(_, rest2) => Success(r1, rest2)
                }
            }
          }
        }
      }
    }

  /* Method of the user API -- Note the reverse of the HLists */
  def project[PS <: HList, PSR <: HList, NS <: HList, NSR <: HList, R <: HList, RR <: HList]
    (ps: PS, ns: NS)(implicit projectable: Projectable[PSR, NSR, R],
     reverseps: Reverse.Aux[PS, PSR], reversens: Reverse.Aux[NS, NSR],
     reverserp: Reverse.Aux[R, RR]): Parser[RR] =
        projectable.project(ps.reverse, ns.reverse) map {
          _.reverse
        }

  /* Method of the user API -- XXX NO REVERSE */
  def project2[PS <: HList, NS <: HList, R <: HList, RR <: HList]
    (ps: PS, ns: NS)(implicit projectable: Projectable[PS, NS, R],
      reverserr: Reverse.Aux[R, RR]): Parser[RR] =
        projectable.project(ps, ns) map {
          _.reverse
        }

}

