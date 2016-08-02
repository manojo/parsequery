package parsec.optimised

import scala.reflect.macros.blackbox.Context
import util.{Zeroval, TreeTools}

/**
 * This trait rewrites grammars according to the Parsequery
 * transformation rules.
 */
trait ParsequeryTransform
    extends GrammarTrees
    with Zeroval
    with TreeTools {

  val c: Context
  import c.universe._


  /**
   * The global, outermost transform function
   * It forwards implementation to `transformMap` if the Grammar we have
   * at hand corresponds to such a grammar.
   */
  def transform(g: Grammar): Grammar = g match {
    case Concat(l, r, t) => Concat(transform(l), transform(r), t)
    case ConcatLeft(l, r, t) => ConcatLeft(transform(l), transform(r), t)
    case ConcatRight(l, r, t) => ConcatRight(transform(l), transform(r), t)
    case Or(l, r, t) => Or(transform(l), transform(r), t)
    case Mapped(g, f, t) => transformMap(g, f, t)
    /* TODO: add rep and repsep */
    case _ => g
  }

  /**
   *  The following rules apply
   *
   *   - Concat_1: Pushing of maps closer to their creation
   *     T[[ (a ~ b) map { case (a, b) => f3(f1(a), f2(b)) } ]] =
   *     (T[[ a ]] map f1 ~ T[[ b ]] map f2) map { case (a, b) => f3(a, b) }
   *
   *     For now, we expect f3 to be either a case class constructor (this case
   *     also covers tuples) or method (which can also be an anonymous functions).
   *
   *   - Concat_2: Turning parsers into recognizers (make them id recognizers for now)
   *     T[[ (a ~ b) map { case (a, b) => f(a) } ]] = T[[ a ]] map f <~ recognize(T[[ b ]])
   *     T[[ (a ~ b) map { case (a, b) => f(b) } ]] = recognize(T[[ a ]]) ~> T[[ b ]] map f
   *
   *   - Map_Map over Parser
   *     T[[ T[[ p map f map g ]]  =  T[[ p ]] map (f andThen g) ]]
   */
  def transformMap(g: Grammar, f: Tree, u: Type): Grammar = g match {
    case Or(l, r, t) =>
      val q"${lMapped: Grammar}" = q"$l.map[$t]($f)"
      val q"${rMapped: Grammar}" = q"$r.map[$t]($f)"
      Or(lMapped, rMapped, t)

    /**
     * map of a map, go under
     * Note: it is not sufficient to just propagate the
     * transformation under, i.e. this is imperfect:
     *     case Mapped(_, _, _) => Mapped(transform(g), f, u)
     *
     * If the result of `transform(g)` is a Concat, we still want
     * to perform `transformMap` on it.
     *
     * If on the other hand `transform(g)` is a Mapped, we will run
     * into an infinite loop by recursively calling ourselves.
     * Option 1: compose functions `f` and `f2` from the inner map
     * Option 2: perform one level of pattern matching deeper
     * Option 2 seems better since it flattens all maps into a single one
     * While this can be done through staging later, we might as well have
     * it done now.
     */
    case Mapped(g1, f2, u2) =>

      val argTerm = TermName(c.freshName("arg"))
      val arg = q"$argTerm"

      val inlinedf2 = inline(f2, List(arg))
      val inlined = inline(f, List(inlinedf2))

      val composed = q"($argTerm: ${g1.t}) => $inlined"

      transformMap(g1, composed, u)


    /**
     * The map function will only ever be involved on one side
     * TODO: The right hand side must be turned into a recogniser
     */
    case ConcatLeft(l, r, t)  => ConcatLeft(transformMap(l, f, u), r, t)
    case ConcatRight(l, r, t) => ConcatRight(l, transformMap(r, f, u), u)

    //case Concat(l, r, t) =>

    case _ => Mapped(g, f, u)
  }

}
