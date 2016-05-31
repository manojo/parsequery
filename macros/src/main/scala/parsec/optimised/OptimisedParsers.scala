package parsec.optimised

import parsec._
import util._
import scala.language.experimental.macros
import scala.reflect.macros.blackbox.Context
import scala.annotation.tailrec

trait OptimisedParsers extends CharParsers {

  def optimise[T](parserBlock: => this.Parser[T]): Parser[T] =
    macro OptimisedParsersImpl.optimise

}

class OptimisedParsersImpl(val c: Context) extends StagedGrammars {

  import c.universe._

  //scalastyle:off line.size.limit
  /**
   * A data type representing a parser declaration
   * Inspired by `RuleInfo` in FastParsers
   * @see https://github.com/begeric/FastParsers/blob/experiment/FastParsers/src/main/scala/fastparsers/framework/ruleprocessing/RulesProcessing.scala
   */
   //scalastyle:on line.size.limit

  case class ParserDecl(
    name: TermName,
    tparams: List[Tree],
    retType: Type,
    production: Grammar)

  /**
   * A data type representing all the information inside
   * an `optimise` block
   */
  case class ParserBlock(ruleMap: Map[TermName, ParserDecl], finalStmt: PIdent)

  /**
   * Takes a list of statements that represent code that is
   * declared in the parser block, and constructs a list of
   * `Grammar` trees for all parsers declared in the block
   * including the final one.
   *
   * A parser block can only contain (for now) parser declarations
   *
   */
  def createParserBlock(statements: List[Tree]): ParserBlock = {
    val (stmts, finalParser) = (statements.init, statements.last)

    import scala.collection.mutable.Map
    val ruleMap: Map[TermName, ParserDecl] = Map.empty

    stmts foreach { stmt => stmt match {
      case q"def ${name: TermName}[..$tparams]: ${retType: Type} = ${g: Grammar}"
        if retType <:< parserType =>
          ruleMap += name -> ParserDecl(name, tparams, retType, g)

      case _ => c.abort(
        c.enclosingPosition,
        s"""
        |only parser definitions (with no parameters) are allowed in the `optimise` scope.
        |Yet we got the following:
        |${showCode(stmt)}
        |""".stripMargin
      )
    }}

    val q"${finalG: Grammar}" = finalParser

    /**
     * we create a "normalised" representation here:
     * if `finalG` is not named, then we want to stage it
     * so we create a parser declaration for it, and then
     * transform that one
     */
    val finalExtended: PIdent = finalG match {
      case p @ PIdent(_) => p
      case _ =>
        val finalPName = c.freshName(TermName("finalParser"))
        ruleMap += (finalPName -> ParserDecl(finalPName, Nil, finalParser.tpe, finalG))
        PIdent(Ident(finalPName))
    }

    ParserBlock(ruleMap.toMap, finalExtended)
  }

  /**
   * transforms a parser block by rewriting optimising each parser
   * in it. Yields a tree at the end.
   * For now, we return the identity transform, modulo de-sugarings
   */
  def transform(pBlock: ParserBlock): Tree = {

    import scala.collection.mutable.{Map => MuMap}
    val oldToNew: MuMap[Name, Name] = MuMap.empty

    val ParserBlock(ruleMap, finalG) = pBlock

    /**
     * we first stage each parser we see, i.e.
     * Convert a Parser (Rep[Input => ParseResult]) into
     * a Rep[Input] => Rep[ParseResult]
     */
    val stagedParsers: Map[TermName, Option[Parser]] = {
      for ((name, ParserDecl(_, _, _, g)) <- ruleMap)
      yield (name -> stage(g))
    }

    /**
     * creating a Parser back!
     */
    val functionalised: Map[TermName, Option[Tree]] = {
      for ((name, optionP) <- stagedParsers) yield {
        val mapped = optionP map { parser =>
          val inputTerm = TermName(c.freshName("input"))
          val in = q"$inputTerm"
          q"Parser { ($in: Input) => ${parser(in).toParseResult} }"
        }
        (name -> mapped)
      }
    }

    /**
     * for each parser definition in scope we create a new, but identical
     * definition. We cannot re-use the same name as before since we are creating
     * a new definition, hence the need to create a new TermName.
     *
     * Having created a new termname, we naturally need to propagate the
     * changes to any place these might be used. The changes are accumulated
     * in the `oldToNew` map.
     *
     * Currently we only check the final parser statement of the block.
     */
    val stmts: List[Tree] = {
      (for ((_, ParserDecl(name, tparams, retType, g)) <- ruleMap) yield {

        println("return type")
        println(retType)

        val TermName(nameString) = name
        val tmpParserName = c.freshName(TermName(nameString))
        oldToNew += (name -> tmpParserName)

        functionalised.get(name) match {
          case Some(Some(parser)) => q"def $tmpParserName[..$tparams]: $retType = $parser"
          case _            => q"def $tmpParserName[..$tparams]: $retType = $g"
        }
      }).toList
    }

    /**
     * rewriting the final grammar in case it refers now to an old
     * name
     */
    val newFinalG: Grammar = finalG match {
      case PIdent(Ident(tname)) => oldToNew.get(tname) match {
        case Some(newName) => PIdent(Ident(newName))
        case _ => finalG
      }
      case _ => finalG
    }

    q"""{
      ..$stmts
      $newFinalG
    }"""
  }

  /**
   * This method is the macro entry point. TODO: should it be typed
   * so that it is a c.Expr[Parser[T]]?
   */
  def optimise(parserBlock: c.Tree) = parserBlock match {
    case q"{..$statements}" =>
      println("BEFORE TRANSFORMATION")
      println(showCode(parserBlock))

      val pBlock = createParserBlock(statements)
      val transformed = transform(pBlock)

      println("AFTER TRANSFORMATION")
      println(showCode(transformed))
      println()
      transformed

    case _ =>
      println(showCode(parserBlock))
      c.abort(c.enclosingPosition, "the body does not match anything we expect")
  }
}
