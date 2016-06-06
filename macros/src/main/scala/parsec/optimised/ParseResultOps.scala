package parsec.optimised

import scala.reflect.macros.blackbox.Context
import util.Zeroval

/**
 * A ParseResult needs to know of a CPS encoding for a CharReader
 * We specialise to `CharReader` since parsers above already do it too.
 * TODO: abstract as and when needed.
 */
trait ParseResultOps { self: ReaderOps with Zeroval =>

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
    def apply(success: (Tree, CharReader) => Tree, failure: CharReader => Tree): Tree

    def map(t: Type, f: Tree => Tree) = new ParseResult(t) {
      /**
       * we make a join point right away, and avoid all the mess
       * later
       */
      def apply(success: (Tree, CharReader) => Tree, failure: CharReader => Tree): Tree = {
        val isSuccessTerm = TermName(c.freshName("success"))
        val isSuccess = q"$isSuccessTerm"

        val sourceTerm = TermName(c.freshName("source"))
        val source = q"$sourceTerm"

        val tmpPosTerm = TermName(c.freshName("tmpPos"))
        val tmpPos = q"$tmpPosTerm"

        val tmpResTerm = TermName(c.freshName("tmpRes"))
        val tmpRes = q"$tmpResTerm"

        val applied = self.apply(
          (res, rest) => {

            val readerApplied = rest.apply {
              (src, pos) => q"$source = $src; $tmpPos = $pos"
            }

            q"""
              $tmpRes = $res
              $readerApplied
              $isSuccess = true
            """
          },

          rest => rest.apply {
            (src, pos) => q"$source = $src; $tmpPos = $pos"
          }
        )

        q"""
          var $isSuccessTerm: Boolean = false
          var $tmpResTerm: ${self.elemType} = ${zeroValue(self.elemType)}
          var $sourceTerm: Array[Char] = null
          var $tmpPosTerm: Int = 0

          $applied

          if ($isSuccess) ${success(f(tmpRes), mkCharReader(source, tmpPos)) }
          else ${failure(mkCharReader(source, tmpPos))}
        """
      }
    }

    def flatMapWithNext(t: Type, f: Tree => CharReader => ParseResult) =  {
      new ParseResult(t) {
        def apply(success: (Tree, CharReader) => Tree, failure: CharReader => Tree) = {
          val isSuccessTerm = TermName(c.freshName("success"))
          val isSuccess = q"$isSuccessTerm"

          val sourceTerm = TermName(c.freshName("source"))
          val source = q"$sourceTerm"

          val tmpPosTerm = TermName(c.freshName("tmpPos"))
          val tmpPos = q"$tmpPosTerm"

          val tmpResTerm = TermName(c.freshName("tmpRes"))
          val tmpRes = q"$tmpResTerm"

          val applied = self.apply(
            (res, rest) => {

              val readerApplied = rest.apply {
                (src, pos) => q"$source = $src; $tmpPos = $pos"
              }

              q"""
                $tmpRes = $res
                $readerApplied
                $isSuccess = true
              """
            },
            rest => rest.apply {
              (src, pos) => q"$source = $src; $tmpPos = $pos"
            }
          )

          q"""
            var $isSuccessTerm: Boolean = false
            var $tmpResTerm: ${self.elemType} = ${zeroValue(self.elemType)}
            var $sourceTerm: Array[Char] = null
            var $tmpPosTerm: Int = 0

            $applied

            if ($isSuccess) ${f(tmpRes)(mkCharReader(source, tmpPos)).apply(success, failure)}
            else ${failure(mkCharReader(source, tmpPos))}
          """
        }
      }
    }

    def orElse(t: Type, that: ParseResult) = new ParseResult(t) {
      val isSuccessTerm = TermName(c.freshName("success"))
      val isSuccess = q"$isSuccessTerm"

      val sourceTerm = TermName(c.freshName("source"))
      val source = q"$sourceTerm"

      val tmpPosTerm = TermName(c.freshName("tmpPos"))
      val tmpPos = q"$tmpPosTerm"

      val tmpResTerm = TermName(c.freshName("tmpRes"))
      val tmpRes = q"$tmpResTerm"

      val applied = self.apply(
        (res, rest) => {

          val readerApplied = rest.apply {
            (src, pos) => q"$source = $src; $tmpPos = $pos"
          }

          q"""
            $isSuccess = true
            $tmpRes = $res
            $readerApplied
          """
        },
        rest => q"()" // we don't need to do anything in this case
      )

      def apply(success: (Tree, CharReader) => Tree, failure: CharReader => Tree) = {
        q"""
        var $isSuccessTerm: Boolean = false
        var $sourceTerm: Array[Char] = null
        var $tmpPosTerm: Int = 0
        var $tmpResTerm: ${this.elemType} = ${zeroValue(this.elemType)}

        $applied

        if ($isSuccess) ${success(tmpRes, mkCharReader(source, tmpPos))}
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

      /**
       * This is the only place where we will create an input
       * Otherwise we stay in the CPS encoding
       */
      val inputTerm = TermName(c.freshName("input"))
      val input = q"$inputTerm"

      val applied = this.apply(
        (result, rest) => q"""
          $success = true
          $res = $result
          $input = ${rest.toCharReader}
        """,

        (rest) => q"""
          $success = false
          $input = ${rest.toCharReader}
        """
      )

      q"""
        var $isSuccessTerm: Boolean = false
        var $resTerm: $elemType = ${zeroValue(elemType)}
        var $inputTerm: Input = null
        $applied
        if ($success) Success($res, $input) else Failure("oh noes!", $input)
      """
    }
  }

  def mkSuccess(elemType: Type, elem: Tree, rest: CharReader) = new ParseResult(elemType) {
    def apply(success: (Tree, CharReader) => Tree, failure: CharReader => Tree) =
      success(elem, rest)
  }

  def mkFailure(rest: CharReader) = new ParseResult(typeOf[scala.Nothing]) {
    def apply(success: (Tree, CharReader) => Tree, failure: CharReader => Tree) =
      failure(rest)
  }

  def cond(elemType: Type)(test: Tree, thenp: ParseResult, elsep: ParseResult) = {
    new ParseResult(elemType) {
      def apply(success: (Tree, CharReader) => Tree, failure: CharReader => Tree) = {
        q"""
          if ($test) ${thenp(success, failure)}
          else       ${elsep(success, failure)}
        """
      }
    }
  }
}
