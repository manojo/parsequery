package parsec.optimised

import scala.reflect.macros.blackbox.Context
import util.{Zeroval, TreeTools}

/**
 * This trait contains functionality to stage parser implementations
 * We are using the principle that we have unstaged functions
 * over staged types
 */
trait StagedGrammars
    extends GrammarTrees
    with ParserOps
    with ParseResultOps
    with ReaderOps
    with Zeroval
    with TreeTools {

  val c: Context
  import c.universe._

  /**
   * transforms a grammar into a "staged" parser
   * Rep[Input] => Rep[ParseResult[T]]
   * Since we work with trees in the the universe
   * we have Tree => Tree instead
   * We carry an `oldToNew` map that allows us to replace
   * named parsers with their modified names
   */
  def stage(g: Grammar)(implicit oldToNew: Map[Name, TermName]): Option[Parser] = g match {
    case a @ AcceptIf(_, p)  => Some(acceptIf(a.tpe, p))
    case AcceptStr(_, s) => Some(acceptStr(s))
    case SuccessGrammar(_, t, elem) =>
      Some(mkParser(t, { in => mkSuccess(t, elem, in) }))
    case Number(_) => Some(number)
    case DoubleGrammar(path) => Some(
      mkParser(typeOf[Double], { in => new ParseResult(typeOf[Double]) {
        def apply(success: (Tree, CharReader) => Tree,
                  failure: CharReader => Tree) = {

          val cReader = q"${in.toCharReader}"
          val parsed = q"$path.double(cReader)"

          val tmpResTerm = TermName(c.freshName("tmpRes"))
          val tmpRes = q"$tmpResTerm"

          val tmpSourceTerm = TermName(c.freshName("tmpSource"))
          val tmpSource = q"$tmpSourceTerm"

          val tmpPosTerm = TermName(c.freshName("tmpPos"))
          val tmpPos = q"$tmpPosTerm"

          val isSuccessTerm = TermName(c.freshName("success"))
          val isSuccess = q"$isSuccessTerm"

          val inputTerm = TermName(c.freshName("input"))
          val input = q"$inputTerm"

          val resTerm = TermName(c.freshName("doubleRes"))
          val res = q"$resTerm"

          val restTerm = TermName(c.freshName("rest"))
          val rest = q"$restTerm"

          q"""
            var $isSuccessTerm: Boolean = false
            var $tmpSourceTerm: Array[Char] = null
            var $tmpPosTerm: Int = 0
            var $tmpResTerm: Double = ${zeroValue(typeOf[Double])}

            val $inputTerm = ${in.toCharReader}
            (..$path).double($input) match {
              case Success($resTerm, $restTerm) =>
                $tmpSource = $rest.source; $tmpPos = $rest.pos; $tmpRes = $res
                $isSuccess = true

              case Failure(_, $restTerm) =>
                $tmpSource = $rest.source; $tmpPos = $rest.pos
            }

            if ($isSuccess) ${success(tmpRes, mkCharReader(tmpSource, tmpPos))}
            else ${failure(mkCharReader(tmpSource, tmpPos))}
          """
        }


      }}
    ))
    case StringLiteral(_) => Some(stringLiteral)

    /** TODO: should this be desugared before showing up here? */
    case SkipWs(_, t, g) => for (f <- stage(g)) yield skipWs(f)

    case Rep(g, t) => for (f <- stage(g)) yield rep(t, f)

    case Concat(l, r, t) => for {
      lp <- stage(l)
      rp <- stage(r)
    } yield (lp ~ rp)

    case ConcatLeft(l, r, t) => for {
      lp <- stage(l)
      rp <- stage(r)
    } yield (lp <~ rp)

    case ConcatRight(l, r, t) => for {
      lp <- stage(l)
      rp <- stage(r)
    } yield (lp ~> rp)

    case Or(l, r, t) => for {
      lp <- stage(l)
      rp <- stage(r)
    } yield (lp.or(t, rp))

    case Repsep(g, sep, t, u) => for {
      lp <- stage(g)
      rp <- stage(sep)
    } yield (repsep(lp, rp))

    /**
     * The `map` takes a `Tree => Tree` function
     * The naive `id` transformation involves
     * eta-expanding.
     */
    case Mapped(g, f, t) => for (p <- stage(g))
      yield p.map(t, t => inline(f, List(t)))

    /**
     * If we see a named parser, staging it boils down to calling the new
     * function name for this parser. This solves recursion issues right away
     * TODO: We could inline the definition of this named parser if it
     * is not recursive.
     */
    case PIdent(t @ Ident(name)) =>
      /**
       * `t.tpe` is `Parser[X]`
       * By this point, we know for sure this is true (the program would
       * have failed earlier otherwise)
       * To get `X` we use the `typeArgs` method.
       */
      val retType: Type = t.tpe.typeArgs.head

      val parser = mkParser(retType, { in => new ParseResult(retType) {
        val newName = oldToNew(name)

        val successResTerm  = TermName(c.freshName("res"))
        val successRes  = q"$successResTerm"
        val successRestTerm = TermName(c.freshName("rest"))
        val successRest = q"$successRestTerm"
        val failureRestTerm = TermName(c.freshName("rest"))
        val failureRest = q"$failureRestTerm"

        val inputTerm = TermName(c.freshName("input"))
        val input = q"$inputTerm"

        def apply(success: (Tree, CharReader) => Tree, failure: CharReader => Tree) = q"""
          val $inputTerm = ${in.toCharReader}
          $newName($input) match {
            case Success($successResTerm, $successRestTerm) =>
              ${success(successRes, mkCharReader(q"$successRest.source", q"$successRest.pos"))}
            case Failure(_, $failureRestTerm) =>
              ${failure(mkCharReader(q"$failureRest.source", q"$failureRest.pos"))}
          }
        """
      }})

      Some(parser)

    /**
     * the default case
     */
    case _ => None
  }
}
