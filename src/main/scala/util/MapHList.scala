package util

import shapeless._
import ops.hlist._

/**
 * Mapping over a HList
 * This is different from the `map` in shapeless is
 * that we have a very specific function we want to apply
 * to every element. So we must provide an HList of functions
 * `L` is the input list
 * `R` is the list of functions
 */
trait Mappable[L <: HList, Funs <: HList] {

  /** The Return type of the list */
  type Out <: HList

  def mapAll(ls: L, funs: Funs): Out
}

object Mappable {

  /**
   * It's good to have an Aux type we have access to if need be.
   * Follows the shapeless pattern
   */
  type Aux[L <: HList, Funs <: HList, R <: HList] = Mappable[L, Funs] {
    type Out = R
  }

  /**
   * Mapping a list with a single element does the right thing
   */
  implicit def mapSingleElem[T, U]: Mappable.Aux[T :: HNil, (T => U) :: HNil, U :: HNil] = {
    new Mappable[T :: HNil, (T => U) :: HNil] {
      type Out = U :: HNil

      def mapAll(ls: T :: HNil, funs: (T => U) :: HNil) =
        funs.head(ls.head) :: HNil
    }
  }

  /**
   * Mapping works recursively too.
   */
  implicit def mapRec[T, U, TS <: HList, Funs <: HList, Res <: HList](
                      implicit mappable: Mappable.Aux[TS, Funs, Res]
                     ): Mappable.Aux[T :: TS, (T => U) :: Funs, U :: Res] = {
    new Mappable[T :: TS, (T => U) :: Funs] {
      type Out = U :: Res

      def mapAll(ls: T :: TS, funs: (T => U) :: Funs) =
        funs.head(ls.head) :: mappable.mapAll(ls.tail, funs.tail)
    }
  }

  /**
   * The actual function that will be used
   */
  def map[L <: HList, Funs <: HList, Res <: HList]
         (ls: L, funs: Funs)
         (implicit mappable: Mappable.Aux[L, Funs, Res]): Res = {
    mappable.mapAll(ls, funs)
  }

}
