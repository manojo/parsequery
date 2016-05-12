package parsec.optimised

import scala.reflect.macros.blackbox.Context
import util.Zeroval

trait ParseResultOps { self: Zeroval =>

  val c: Context
  import c.universe._

  /**
   * a CPS-encoded implementation of the ParseResult datatype
   * based on https://github.com/manojo/functadelic/blob/master/src/main/scala/stagedparsec/ParseResultCPS.scala
   *
   * for reminder sake:
   *   ParseResultCPS[T] =
   *     forall X. (success: (T, Input) => X, failure: (Input) => X) => X
   */
  abstract class ParseResult(val elemType: Type) {
    def apply(success: (Tree, Tree) => Tree, failure: Tree => Tree): Tree

    def toParseResult: Tree = {
      /**
       * A parser returns a CPS-encoded ParseResult,
       * we have to inline it here.
       */
      val successTerm = TermName(c.freshName("success"))
      val success = q"$successTerm"

      val resTerm = TermName(c.freshName("res"))
      val res = q"$resTerm"

      val inputTerm = TermName(c.freshName("input"))
      val input = q"$inputTerm"

      val applied = this.apply(
        (result, rest) => q"""
          $success = true
          $res = $result
          $input = $rest
        """,

        (rest) => q"""
          $success = false
          $input = $rest
        """
      )

      val inputType = tq"Input".tpe
      q"""
        var $successTerm = false
        var $resTerm: $elemType = ${zeroValue(elemType)}
        var $inputTerm: Input = null
        $applied
        if ($success) Success($res, $input) else Failure("oh noes!", $input)
      """
    }
  }

  def mkSuccess(elemType: Type, elem: Tree, rest: Tree) = new ParseResult(elemType) {
    def apply(success: (Tree, Tree) => Tree, failure: Tree => Tree) =
      success(elem, rest)
  }

  def mkFailure(rest: Tree) = new ParseResult(typeOf[scala.Nothing]) {
    def apply(success: (Tree, Tree) => Tree, failure: Tree => Tree) =
      failure(rest)
  }

  def cond(elemType: Type)(test: Tree, thenp: ParseResult, elsep: ParseResult) = {
    new ParseResult(elemType) {
      def apply(success: (Tree, Tree) => Tree, failure: Tree => Tree) = {
        q"""
          if ($test) ${thenp(success, failure)}
          else       ${elsep(success, failure)}
        """
      }
    }
  }
}
