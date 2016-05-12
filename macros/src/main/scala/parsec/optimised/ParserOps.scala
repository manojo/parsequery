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
  abstract class Parser(val elemType: Type) extends (Tree => ParseResult)

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


  /**
   * a `FoldParser` represents a ``late'' repetition parser
   * it eventually yields a parser that folds into a collection
   */
  abstract class FoldParser(elemType: Type) { self =>

    /**
     * fold creates a Parser[retType]
     */
    def fold(
      retType: Tree,
      z: Tree,
      combine: (Tree, Tree) => Tree): Parser

    def toListBuffer: Parser = {
      val listBufferType: Tree
        = tq"scala.collection.mutable.ListBuffer[$elemType]"

      val folded = self.fold(
        listBufferType,
        q"scala.collection.mutable.ListBuffer.empty[$elemType]",
        (acc, elem) => q"{ $acc += $elem; $acc }"
      )
      folded
    }
  }
/*
  def fromParser(elemType: Type, parser: Tree => Tree) = new FoldParser(elemType) {
    def fold(
        retType: Tree,
        z: Tree,
        combine: (Tree, Tree) => Tree): Parser = {

      val accTerm = TermName(c.freshName("acc"))
      val acc = q"$accTerm"

      val tmpRes = TermName(c.freshName("tmpRes"))
      val innerRes = TermName(c.freshName("innerRes"))

      val tmpInTerm = TermName(c.freshName("in"))
      val tmpIn = q"$tmpInTerm"

      val resTerm = TermName(c.freshName("res"))
      val res = q"resTerm"
      val rest = TermName(c.freshName("rest"))

      println("Elem type is")
      println(elemType =:= typeOf[scala.Char])

      mkParser(retType, { in => {
        q"""
        var $accTerm: $retType = $z
        var $tmpInTerm = $in

        var $tmpRes: ParseResult[$elemType]
          = Success(${zeroValue(elemType)}, $tmpInTerm)

        while ($tmpRes.isSuccess) {
          val $innerRes = ${parser(tmpIn)}
          if ($innerRes.isSuccess) {
            val Success($res, $rest) = $innerRes
            $acc = ${combine(acc, res)}
            $tmpIn = $rest
          }
          $tmpRes = $innerRes
        }

        Success($acc, $tmpIn)
        """
      }})
    }
  }
  */
}
