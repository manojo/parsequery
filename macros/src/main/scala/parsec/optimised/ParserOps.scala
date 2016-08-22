package parsec.optimised

import scala.reflect.macros.blackbox.Context
import util.Zeroval

/**
 * A staged representation for parsers
 * Note that we have already specialised the `Elem` type
 * to be Char here (since we imlpement a lot of char-specific)
 * parsers
 *
 * TODO: when needed, we need to abstract one level more, so as
 * to specialise as needed.
 */
trait ParserOps { self: ParseResultOps with ReaderOps with Zeroval =>

  val c: Context
  import c.universe._

  /**
   * a CPS-encoded implementation of the Parser datatype
   * based on https://github.com/manojo/functadelic/blob/master/src/main/scala/stagedparsec/StagedParsers.scala
   */
  abstract class Parser(val elemType: Type) extends (CharReader => ParseResult) {
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

    def or(t: Type, that: Parser) = mkParser(t, { in =>
      this(in).orElse(t, that(in))
    })
  }

  def mkParser(elemType: Type, f: CharReader => ParseResult) = new Parser(elemType) {
    def apply(in: CharReader) = f(in)
  }

  def acceptIf(elemType: Type, p: Tree => Tree): Parser = mkParser(elemType, { in =>
    cond(elemType)(in.atEnd,
      mkFailure(in),
      cond(elemType)(q"${p(in.first)}",
        mkSuccess(elemType, in.first, in.rest),
        mkFailure(in)
      )
    )
  })

  def accept(c: Char) = acceptIf(typeOf[Char], elem => q"$elem == $c")

  def rep(elemType: Type, p: Parser): Parser = {
    val listType = appliedType(typeOf[List[_]], List(elemType))
    fromParser(elemType, p).toListBuffer.map(listType, lb => q"$lb.toList")
  }

  def repsep(p: Parser, sep: Parser): Parser = {
    val foldable = new FoldParser(p.elemType) {
      def fold(
          retType: Type,
          z: Tree,
          combine: (Tree, Tree) => Tree): Parser = {

        mkParser(retType, { in => new ParseResult(retType) {

          def apply(success: (Tree, CharReader) => Tree, failure: CharReader => Tree) = {
            p(in).apply(
              /**
               * if `p` succeeds once, we can repeat `sep ~> p`,
               * to possibly combine more into it.
               */
              (res, rest) => {
                val tmpAccTerm = TermName(c.freshName("tmpAcc"))
                val tmpAcc = q"$tmpAccTerm"

                val foldedRest: Parser = fromParser(p.elemType, sep ~> p).fold(
                  retType,
                  q"val $tmpAccTerm = $z; ${combine(tmpAcc, res)}",
                  combine)
                foldedRest(rest).apply(success, failure)
              },

              /**
               * if `p` fails, we can still return the `z` element
               */
              rest => success(z, in)
            )
          }
        }})
      }
    }

    val listType = appliedType(typeOf[List[_]], List(p.elemType))
    foldable.toListBuffer.map(listType, lb => q"$lb.toList")
  }

  def acceptStr(s: String): Parser = {
    val strLen = s.length

    mkParser(typeOf[String], { in => new ParseResult(typeOf[String]) {
      def apply(success: (Tree, CharReader) => Tree, failure: CharReader => Tree) = {
        val strArrTerm = TermName(c.freshName("strArr"))
        val strArr = q"$strArrTerm"

        val curIdxTerm = TermName(c.freshName("curIdx"))
        val curIdx = q"$curIdxTerm"

        val sourceTerm = TermName(c.freshName("source"))
        val source = q"$sourceTerm"

        val tmpPosTerm = TermName(c.freshName("tmpPos"))
        val tmpPos = q"$tmpPosTerm"

        val continueTerm = TermName(c.freshName("continue"))
        val continue = q"$continueTerm"

        val isSuccessTerm = TermName(c.freshName("success"))
        val isSuccess = q"$isSuccessTerm"

        val tmpReader = mkCharReader(source, tmpPos)
        val updatePosition = tmpReader.rest.apply((src, pos) => q"$tmpPos = $pos")

        q"""
          val $strArrTerm: Array[Char] = $s.toArray
          var $curIdxTerm: Int = 0
          var $sourceTerm: Array[Char] = ${in.getSource}
          var $tmpPosTerm: Int = ${in.getPos}
          var $continueTerm: Boolean = true
          var $isSuccessTerm: Boolean = false

          while ($continue) {
            if ($curIdx >= $strLen) { $continue = false; $isSuccess = true }
            else if (${tmpReader.atEnd}) { $continue = false }
            else if (${tmpReader.first} != $strArr($curIdx)) { $continue = false }
            else { $curIdx += 1; $updatePosition }
          }

          if ($isSuccess) ${success(q"$s", tmpReader)}
          else ${failure(in)}

        """
      }
    }})
  }

  /**
   * A whitespace parser that folds into unit
   */
  def ws = fromParser(
    typeOf[Char],
    accept(' ').or(typeOf[Char], accept('\n'))
  ).toSkipper

  def skipWs(p: Parser) = ws ~> p <~ ws

  /**
   * other helpful parsers
   */
  def digit = acceptIf(typeOf[Char], elem => q"$elem.isDigit")
  def digit2Int = digit.map(typeOf[Int], elem => q"($elem - '0').toInt")
  def number = digit2Int.flatMap(typeOf[Int], d =>
    fromParser(typeOf[Int], digit2Int).fold(
      typeOf[Int],
      d,
      (acc, elem) => q"$acc * 10 + $elem"
    )
  )

  def stringLiteral = {
    import scala.collection.mutable.StringBuilder

    def legitCharacter = (accept('\\') ~> acceptIf(typeOf[Char], elem => q"true")).or(
      typeOf[Char], acceptIf(typeOf[Char], c => q"""$c != '"'""")
    )

    (accept('"') ~>
      fromParser(typeOf[Char], legitCharacter).fold(
        typeOf[StringBuilder],
        q"scala.collection.mutable.StringBuilder.newBuilder",
        (acc, elem) => q"$acc.append($elem)"
      )
    <~ accept('"')).map(typeOf[String], (sbuf => q"$sbuf.toString"))
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

    /**
     * folds into a list buffer
     */
    def toListBuffer: Parser = {
      val listBufferType = appliedType(
        typeOf[scala.collection.mutable.ListBuffer[_]],
        List(elemType))
//        c.typecheck(tq"scala.collection.mutable.ListBuffer[$elemType]",
//          c.TYPEmode).tpe

      val folded = self.fold(
        listBufferType,
        q"scala.collection.mutable.ListBuffer.empty[$elemType]",
        (acc, elem) => q"{ $acc += $elem; $acc }"
      )
      folded
    }

    /**
     * folds into unit
     */
    def toSkipper: Parser = self.fold(
      typeOf[scala.Unit],
      q"()",
      (acc, elem) => acc
    )
  }

  def fromParser(elemType: Type, parser: Parser) = new FoldParser(elemType) {
    def fold(
        retType: Type,
        z: Tree,
        combine: (Tree, Tree) => Tree): Parser = {

      val accTerm = TermName(c.freshName("acc"))
      val acc = q"$accTerm"

      val isSuccessTerm = TermName(c.freshName("success"))
      val isSuccess = q"$isSuccessTerm"

      val sourceTerm = TermName(c.freshName("source"))
      val source = q"$sourceTerm"

      val tmpPosTerm = TermName(c.freshName("tmpPos"))
      val tmpPos = q"$tmpPosTerm"

      val resTerm = TermName(c.freshName("res"))
      val res = q"resTerm"

      val applied = parser(mkCharReader(source, tmpPos)).apply(
        (innerRes, rest) => {

          val readerApplied = rest.apply {
            (src, pos) => q"$source = $src; $tmpPos = $pos"
          }

          q"""
            $acc = ${combine(acc, innerRes)}
            $readerApplied
          """
        },
        (rest) => q"$isSuccess = false"
      )

      mkParser(retType, { in => new ParseResult(retType) {

        /**
         * we know in this case that we have a success, so
         * we'll hard-code the calling of the success continuation
         */
        def apply(success: (Tree, CharReader) => Tree, failure: CharReader => Tree) = q"""
          var $accTerm: $retType = $z
          var $sourceTerm: Array[Char] = ${in.getSource}
          var $tmpPosTerm: Int = ${in.getPos}
          var $isSuccessTerm: Boolean = true

          while ($isSuccess) {
            $applied
          }
          ${success(acc, mkCharReader(source, tmpPos))}
        """
      }})
    }
  }
}
