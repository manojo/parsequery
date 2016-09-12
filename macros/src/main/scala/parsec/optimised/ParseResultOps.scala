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
        self.apply(
          (res, rest) => {
            /**
             * We must compute the result at this point,
             * and pass a reference to it.
             */
            val tmpResTerm = TermName(c.freshName("res"))
            val tmpRes = q"$tmpResTerm"
            q"""
              val $tmpResTerm = ${f(res)}
              ${success(tmpRes, rest)}
            """
          },
          failure
        )
      }
    }

    def flatMapWithNext(t: Type, f: Tree => CharReader => ParseResult) = {
      new ParseResult(t) {
        def apply(success: (Tree, CharReader) => Tree, failure: CharReader => Tree) = {
          self.apply(
            (res, rest) => f(res)(rest).apply(success, failure),
            failure
          )
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

      val successK = (res: Tree, rest: CharReader) => {
        val readerApplied = rest.apply {
          (src, pos) => q"$source = $src; $tmpPos = $pos"
        }

        q"""
        $isSuccess = true
        $tmpRes = $res
        $readerApplied
        """
      }

      val failureK = (rest: CharReader) => rest.apply { (src, pos) =>
        q"""$source = $src; $tmpPos = $pos"""
      }

      val applied = self.apply(successK, failureK)
      val conditioned = cond(t)(
        q"$isSuccess",
        mkSuccess(this.elemType, tmpRes, mkCharReader(source, tmpPos)),
        that
      )

      def apply(success: (Tree, CharReader) => Tree, failure: CharReader => Tree) = {
        q"""
        var $isSuccessTerm: Boolean = false
        var $sourceTerm: Array[Char] = null
        var $tmpPosTerm: Int = 0
        var $tmpResTerm: ${this.elemType} = ${zeroValue(this.elemType)}

        $applied
        ${conditioned.apply(success, failure)}
        """
      }
    }

    def toParseResult: Tree = this.apply(
      (result, rest) => q"""Success($result, ${rest.toCharReader})""",
      rest => q"""Failure("oh noes!", ${rest.toCharReader})"""
    )
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

        val isSuccessTerm = TermName(c.freshName("success"))
        val isSuccess = q"$isSuccessTerm"

        val resTerm = TermName(c.freshName("res"))
        val res = q"$resTerm"

        val sourceTerm = TermName(c.freshName("source"))
        val source = q"$sourceTerm"

        val tmpPosTerm = TermName(c.freshName("tmpPos"))
        val tmpPos = q"$tmpPosTerm"

        val successK = (result: Tree, rest: CharReader) => {

          val readerApplied = rest.apply {
            (src, pos) => q"$source = $src; $tmpPos = $pos"
          }

          q"""
            $isSuccess = true
            $res = $result
            $readerApplied
          """
        }

        val failureK = (rest: CharReader) => rest.apply {
          (src, pos) => q"$source = $src; $tmpPos = $pos"
        }

        val applied = q"""
          if ($test) ${ thenp.apply(successK, failureK) }
          else       ${ elsep.apply(successK, failureK) }
        """

        q"""
          var $isSuccessTerm: Boolean = false
          var $resTerm: ${this.elemType} = ${zeroValue(this.elemType)}
          var $sourceTerm: Array[Char] = null
          var $tmpPosTerm: Int = 0

          $applied

          if ($isSuccess) ${success(res, mkCharReader(source, tmpPos))}
          else            ${failure(mkCharReader(source, tmpPos))}
        """
      }
    }
  }
}
