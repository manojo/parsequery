package util

import Functors._
import scala.annotation.tailrec

/**
 * Just the usual fold grammar
 */
abstract class Foldable[+T, F[_]: Functor] extends java.io.Serializable { self =>

  def fold[R](z: R, combine: (R, T) => R): F[R]

  /**
   * map. Pretty nice, cause we can forward the map
   * function over to the underlying parser, it's exactly
   * the same!
   */
  def map[U](f: T => U) = new Foldable[U, F] {
    def fold[R](z: R, combine: (R, U) => R): F[R] = self.fold(
      z,
      (acc: R, elem: T) => combine(acc, f(elem))
    )
  }

  /**
   * filter
   */
  def filter(p: T => Boolean) = new Foldable[T, F] {
    def fold[R](z: R, comb: (R, T) => R) = self.fold(
      z,
      (acc: R, elem: T) => if (p(elem)) comb(acc, elem) else acc
    )
  }

  /**
   * flatMap. It is unclear what semantics this should have for now
   * let's implement it later
   */
  /*def flatMap[U](f: T => CPSList[U, R]) = new Foldable[U, R] {

    def fold(z: R, comb: Combine[U, R]) = self.fold(
      z,
      (acc: R, elem: T) => {
        val nestedList = f(elem)
        nestedList.fold(acc, comb)
      }
    )
  }*/

  /**
   * partition
   * This will create code what will run through the original fold twice
   * once for the positive predicate, once for the negative.
   *
   * see the following related post: http://manojo.github.io/2015/03/03/staged-foldleft-partition/
   */
  def partition(p: T => Boolean): (Foldable[T, F], Foldable[T, F]) = {
    val trues = this filter p
    val falses = this filter (a => !p(a))
    (trues, falses)
  }

  /**
   * partfition, that produces a CPSList over `Either` instead of
   * two `CPSList`s. The important thing is to keep the one
   * CPSList abstraction.
   * This can be rewritten using `map`.
   * see the following related post: http://manojo.github.io/2015/03/12/staged-foldleft-groupby/
   */
  def partitionBis(p: T => Boolean) =
    this map (elem => if (p(elem)) Left(elem) else Right(elem))

  /**
   * groupWith
   * takes a function which computes some grouping property
   * does not create groups just yet, just propagates key-value pairs
   *
   * can be rewritten using `map`.
   * see the following related post: http://manojo.github.io/2015/03/12/staged-foldleft-groupby/
   */
  def groupWith[K](f: T => K): Foldable[(K, T), F] =
    this map (elem => (f(elem), elem))

  /**
   * utility functions that make it easier to write fold-like functions
   */
  def toListF[U >: T]: F[List[U]] = toListBufferF map (_.toList)
  def toMyListF[U >: T]: F[MyList[U]] = toListBufferF map MyList.bufferToMyList

  import scala.collection.mutable.ListBuffer
  def toListBufferF[U >: T]: F[ListBuffer[U]] = {
    val folded: F[ListBuffer[U]] = self.fold[ListBuffer[U]](
      ListBuffer.empty[U],
      (acc: ListBuffer[U], t: U) => { acc += t; acc }
    )
    folded
  }

  import scala.collection.mutable.ArrayBuffer
  def toArrayBufferF[U >: T]: F[ArrayBuffer[U]] = {
    val folded: F[ArrayBuffer[U]] = self.fold[ArrayBuffer[U]](
      ArrayBuffer.empty[U],
      (acc: ArrayBuffer[U], t: U) => { acc += t; acc }
    )
    folded
  }

  def toSkipper: F[Unit] = self.fold((), (acc: Unit, _) => acc)
  def toLength: F[Int] = self.fold(0, (acc: Int, _) => acc + 1)
}

/**
 * A simple self-contained list-like collection. This is easier to
 * handle in the `optimise` macro than the full-blown list. And also
 * carries a more restrictive interface, which is helpful
 */
abstract class MyList[+T] extends Foldable[T, Functors.Id] {
  def fold[R](z: R, combine: (R, T) => R): R = {
    @tailrec
    def loop(acc: R, tmpLs: MyList[T]): R = tmpLs match {
      case Nil => acc
      case Cons(x, xs) => loop(combine(acc, x), xs)
    }

    loop(z, this)
  }

  def append[U >: T](elem: U): MyList[U] = this match {
    case Nil => Cons(elem, Nil)
    case Cons(x, xs) => Cons(x, xs.append(elem))
  }
}

object MyList {
  import scala.collection.mutable.ListBuffer
  def bufferToMyList[T](lb: ListBuffer[T]): MyList[T] =
    lb.toList.foldRight(apply[T]())((x, xs) => Cons(x, xs))

  def apply[T](): MyList[T] = Nil
}

case object Nil extends MyList[Nothing]
case class Cons[+T](x: T, xs: MyList[T]) extends MyList[T]
