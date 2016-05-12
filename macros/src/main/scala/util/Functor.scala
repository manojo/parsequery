package util

/**
 * An object containing the Functor typeclass
 */
object Functors {
  type Id[+T] = T

  /**
   * F has a map on it
   */
  abstract class Functor[F[_]] {
    def map[T, U](f: T => U): F[T] => F[U]
  }

  implicit object IdFunctor extends Functor[Id] {
    def map[T, U](f: T => U) = (x: T) => f(x)
  }

  /**
   * inspired from
   * https://www.safaribooksonline.com/blog/2013/05/28/scala-type-classes-demystified/
   */
  implicit class FOps[F[_]: Functor, T](t: F[T]) {
    val witness = implicitly[Functor[F]]
    def map[U](f: T => U): F[U] = witness.map[T, U](f)(t)
  }
}
