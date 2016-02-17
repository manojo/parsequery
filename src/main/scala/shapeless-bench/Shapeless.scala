package shapeless

import shapeless._
import poly._

/**
 * This source code is compiled by an embedded compiler to 
 * check the compilation time of HList for extreme cases.
 *
 * Currently, there is only a dummy test that should be
 * replaced for more concrete and extensive examples of
 * use of parsequery.
 */
object Shapeless {
  def test(a: String, b: Int, c: Double, d: Char) =
    a :: b :: c :: d ::
      a :: b :: c :: d ::
      a :: b :: c :: d ::
      a :: b :: c :: d ::
      a :: b :: c :: d ::
      a :: b :: c :: d ::
      a :: b :: c :: d ::
      a :: b :: c :: d ::
      a :: b :: c :: d :: HNil

  (0 to 100).map(_ => test("foo.bar.baz", 123123, 431.4, 'a'))
}
