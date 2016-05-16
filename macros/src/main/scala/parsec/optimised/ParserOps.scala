package parsec.optimised

import scala.reflect.macros.blackbox.Context
import util.Zeroval

trait ParserOps { self: ParseResultOps with Zeroval =>

  val c: Context
  import c.universe._

  /**
   * a CPS-encoded implementation of the Parser datatype
   * based on https://github.com/manojo/functadelic/blob/master/src/main/scala/stagedparsec/StagedParsers.scala
   */
  abstract class Parser(val elemType: Type) extends (Tree => ParseResult) {
    def map(t: Type, f: Tree => Tree) = mkParser(t, { in =>
      this(in).map(t, f)
    })

    def flatMap(t: Type, f: Tree => Parser) = mkParser(t, { in =>
      this(in).flatMapWithNext(t, elem => in => f(elem)(in))
    })

    def ~(that: Parser) = {
      val pairType
        = appliedType(typeOf[Tuple2[_, _]], List(elemType, that.elemType))

      this.flatMap(pairType, { l =>
        that.map(pairType, r => q"($l, $r)")
      })
    }

    def <~(that: Parser) = this.flatMap(elemType, { l =>
      that.map(elemType, _ => l)
    })

    def ~>(that: Parser) = this.flatMap(that.elemType, { _ =>
      that.map(that.elemType, r => r)
    })

  }

  def mkParser(elemType: Type, f: Tree => ParseResult) = new Parser(elemType) {
    def apply(in: Tree) = f(in)
  }

  def acceptIf(elemType: Type, p: Tree): Parser = mkParser(elemType, { (in) =>
    cond(elemType)(q"$in.atEnd",
      mkFailure(in),
      cond(elemType)(q"${p}($in.first)",
        mkSuccess(elemType, q"$in.first", q"$in.rest"),
        mkFailure(in)
      )
    )
  })

  def rep(elemType: Type, p: Parser): Parser = {
    val listType = appliedType(typeOf[List[_]], List(elemType))
    fromParser(elemType, p).toListBuffer.map(listType, lb => q"$lb.toList")
  }

  /**
   * a `FoldParser` represents a ``late'' repetition parser
   * it eventually yields a parser that folds into a collection
   */
  abstract class FoldParser(val elemType: Type) { self =>

    /**
     * fold creates a Parser[retType]
     */
    def fold(
      retType: Type,
      z: Tree,
      combine: (Tree, Tree) => Tree): Parser

    def toListBuffer: Parser = {
      val listBufferType
        = appliedType(typeOf[scala.collection.mutable.ListBuffer[_]], List(elemType))
//        c.typecheck(tq"scala.collection.mutable.ListBuffer[$elemType]",
//          c.TYPEmode).tpe

      val folded = self.fold(
        listBufferType,
        q"scala.collection.mutable.ListBuffer.empty[$elemType]",
        (acc, elem) => q"{ $acc += $elem; $acc }"
      )
      folded
    }
  }

  def fromParser(elemType: Type, parser: Parser) = new FoldParser(elemType) {
    def fold(
        retType: Type,
        z: Tree,
        combine: (Tree, Tree) => Tree): Parser = {

      val accTerm = TermName(c.freshName("acc"))
      val acc = q"$accTerm"

      val successTerm = TermName(c.freshName("success"))
      val isSuccess = q"$successTerm"

      val tmpInTerm = TermName(c.freshName("in"))
      val tmpIn = q"$tmpInTerm"

      val resTerm = TermName(c.freshName("res"))
      val res = q"resTerm"

      val applied = parser(tmpIn).apply(
        (innerRes, rest) =>
          q"""
            $acc = ${combine(acc, innerRes)}
            $tmpIn = $rest
          """,
        (rest) => q"$isSuccess = false"
      )

      mkParser(retType, { in => new ParseResult(retType) {

        /**
         * we know in this case that we have a success, so
         * we'll hard-code the calling of the success continuation
         */
        def apply(success: (Tree, Tree) => Tree, failure: Tree => Tree) = q"""
          var $accTerm: $retType = $z
          var $tmpInTerm = $in
          var $successTerm: Boolean = true

          while ($isSuccess) {
            $applied
          }
          ${success(acc, tmpIn)}

        """
      }})
    }
  }
}
