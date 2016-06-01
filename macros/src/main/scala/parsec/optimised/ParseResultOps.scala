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
  abstract class ParseResult(val elemType: Type) { self =>
    def apply(success: (Tree, Tree) => Tree, failure: Tree => Tree): Tree

    def map(t: Type, f: Tree => Tree) = new ParseResult(t) {
      /**
       * we make a join point right away, and avoid all the mess
       * later
       */
      def apply(success: (Tree, Tree) => Tree, failure: Tree => Tree): Tree = {
        val isSuccessTerm = TermName(c.freshName("success"))
        val isSuccess = q"$isSuccessTerm"

        val inputTerm = TermName(c.freshName("in"))
        val input = q"$inputTerm"

        val tmpResTerm = TermName(c.freshName("tmpRes"))
        val tmpRes = q"$tmpResTerm"

        val applied = self.apply(
          (res, rest) => q"""
            $tmpRes = $res
            $input = $rest
            $isSuccess = true
          """,
          rest => q"$input = $rest"
        )

        q"""
          var $isSuccessTerm: Boolean = false
          var $tmpResTerm: ${self.elemType} = ${zeroValue(self.elemType)}
          var $inputTerm: Input = null

          $applied

          if ($isSuccess) ${success(f(tmpRes), input)}
          else ${failure(input)}
        """
      }
    }

    def flatMapWithNext(t: Type, f: Tree => Tree => ParseResult) =  {
      new ParseResult(t) {
        def apply(success: (Tree, Tree) => Tree, failure: Tree => Tree) = {
          val isSuccessTerm = TermName(c.freshName("success"))
          val isSuccess = q"$isSuccessTerm"

          val inputTerm = TermName(c.freshName("in"))
          val input = q"$inputTerm"

          val tmpResTerm = TermName(c.freshName("tmpRes"))
          val tmpRes = q"$tmpResTerm"

          val applied = self.apply(
            (res, rest) => q"""
              $tmpRes = $res
              $input = $rest
              $isSuccess = true
            """,
            rest => q"$input = $rest"
          )

          q"""
            var $isSuccessTerm: Boolean = false
            var $tmpResTerm: ${self.elemType} = ${zeroValue(self.elemType)}
            var $inputTerm: Input = null

            $applied

            if ($isSuccess) ${f(tmpRes)(input).apply(success, failure)}
            else ${failure(input)}
          """
        }
      }
    }

    def orElse(t: Type, that: ParseResult) = new ParseResult(t) {
      val isSuccessTerm = TermName(c.freshName("success"))
      val isSuccess = q"$isSuccessTerm"

      val inputTerm = TermName(c.freshName("in"))
      val input = q"$inputTerm"

      val tmpResTerm = TermName(c.freshName("tmpRes"))
      val tmpRes = q"$tmpResTerm"

      val applied = self.apply(
        (res, rest) => q"$isSuccess = true; $tmpRes = $res; $input = $rest",
        rest => q"()" // we don't need to do anything in this case
      )

      def apply(success: (Tree, Tree) => Tree, failure: Tree => Tree) = {
        q"""
        var $isSuccessTerm: Boolean = false
        var $inputTerm: Input = null
        var $tmpResTerm: ${this.elemType} = ${zeroValue(this.elemType)}

        $applied

        if ($isSuccess) ${success(tmpRes, input)}
        else ${that.apply(success, failure)}
        """
      }
    }

    def toParseResult: Tree = {
      /**
       * A parser returns a CPS-encoded ParseResult,
       * we have to inline it here.
       */
      val isSuccessTerm = TermName(c.freshName("success"))
      val success = q"$isSuccessTerm"

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
        var $isSuccessTerm: Boolean = false
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
