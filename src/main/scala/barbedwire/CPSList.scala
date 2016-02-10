package barbedwire

import scala.annotation.tailrec

/**
 * An implementation of delayed (or CPS lists)
 * Instead of composing over lists we compose over foldleft
 *
 * original sources:
 * https://github.com/manojo/staged-fold-fusion/blob/master/src/main/scala/barbedwire/CPSList.scala
 * see the following related post: http://manojo.github.io/2015/02/19/staging-foldleft/
 *
 * the type signature of foldLeft is
 *    def foldLeft[A, B](z: B, comb: (B, A) => A)(xs: List[A]) : B
 *
 */

 /**
  * foldLeft is basically a pair of a zero value and a combination function
  *
  * @TODO It would be nice to not have `R` popping out in the parameters here.
  * So that we only need specify the type at the very end.
  * an alternate design is to have `fold` carry it:
  * def fold[R](z: R, comb: Combine[A, R]): R
  */
abstract class CPSList[A, R] { self =>


  /**
   * a type alias for the combination function for
   * foldLeft
   * `A` is the type of elements that pass through the fold
   * `S` is the type that is eventually computed
   */
  type Combine[A, S] = (S, A) => S

  def fold(z: R, comb: Combine[A, R]): R

  /**
   * map
   */
  def map[B](f: A => B) = new CPSList[B, R] {

    def fold(z: R, comb: Combine[B, R]) = self.fold(
      z,
      (acc: R, elem: A) => comb(acc, f(elem))
    )
  }

  /**
   * filter
   */
  def filter(p: A => Boolean) = new CPSList[A, R] {

    def fold(z: R, comb: Combine[A, R]) = self.fold(
      z,
      (acc: R, elem: A) => if (p(elem)) comb(acc, elem) else acc
    )
  }


  /**
   * flatMap
   */
  def flatMap[B](f: A => CPSList[B, R]) = new CPSList[B, R] {

    def fold(z: R, comb: Combine[B, R]) = self.fold(
      z,
      (acc: R, elem: A) => {
        val nestedList = f(elem)
        nestedList.fold(acc, comb)
      }
    )
  }

  /**
   * concat
   */
  def concat(that: CPSList[A, R]) = new CPSList[A, R] {

    def fold(z: R, comb: Combine[A, R]) = {
      val folded: R = self.fold(z, comb)
      that.fold(folded, comb)
    }
  }

  def ++(that: CPSList[A, R]) = this concat that

  /**
   * append
   */
  def append(elem: A) = new CPSList[A, R] {

    def fold(z: R, comb: Combine[A, R]) = {
      val folded: R = self.fold(z, comb)
      comb(folded, elem)
    }
  }

  def :+(elem: A) = this append elem

  /**
   * partition
   * This will create code what will run through the original fold twice
   * once for the positive predicate, once for the negative.
   *
   * see the following related post: http://manojo.github.io/2015/03/03/staged-foldleft-partition/
   */
  def partition(p: A => Boolean): (CPSList[A, R], CPSList[A, R]) = {
    val trues = this filter p
    val falses = this filter (a => !p(a))
    (trues, falses)
  }

  /**
   * partition, that produces a CPSList over `Either` instead of
   * two `CPSList`s. The important thing is to keep the one
   * CPSList abstraction.
   * This can be rewritten using `map`.
   * see the following related post: http://manojo.github.io/2015/03/12/staged-foldleft-groupby/
   */
  def partitionBis(p: A => Boolean) = new CPSList[Either[A, A], R] {
    def fold(z: R, comb: Combine[Either[A, A], R]) = self.fold(
      z,
      (acc: R, elem: A) =>
        if (p(elem)) comb(acc, Left[A, A](elem))
        else comb(acc, Right[A, A](elem))
    )
  }

  /**
   * groupWith
   * takes a function which computes some grouping property
   * does not create groups just yet, just propagates key-value pairs
   *
   * can be rewritten using `map`.
   * see the following related post: http://manojo.github.io/2015/03/12/staged-foldleft-groupby/
   */
  def groupWith[K](f: A => K): CPSList[(K, A), R] =
    this map (elem => (f(elem), elem))

}

/**
 * companion object, makes it easier to
 * construct folds
 */
object CPSList {

  /**
   * create a fold from list
   */
  def fromList[A, R](ls: List[A]) = new CPSList[A, R] {

    def fold(z: R, comb: Combine[A, R]): R = {

      @tailrec
      def loop(curRes: R, curList: List[A]): R = curList match {
        case Nil => curRes
        case hd :: tl => loop(comb(curRes, hd), tl)
      }

      loop(z, ls)

    }
  }

  /**
   * create a fold from a range
   */
  def fromRange[R](a: Int, b: Int) = new CPSList[Int, R] {

    def fold(z: R, comb: Combine[Int, R]) = {

      @tailrec
      def loop(curRes: R, curInt: Int): R =
        if (curInt > b) curRes
        else loop(comb(curRes, curInt), curInt + 1)

      loop(z, a)
    }
  }
}
