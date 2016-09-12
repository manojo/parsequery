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

      println()
      println("Mapped(Mapped(....))")
      println("before")
      println(showRaw(f))
      println(showRaw(f2))
      println()

      val argTerm = TermName(c.freshName("arg"))
      val arg = q"$argTerm"

      val inlinedf2 = inline(f2, List(arg))
      val inlined = inline(f, List(inlinedf2))

      val composed = q"($argTerm: ${g1.tpe}) => $inlined"

      println("after")
      println(showRaw(composed))

      transformMap(g1, composed, u)


    /**
     * The map function will only ever be involved on one side
     * TODO: The right/left hand sides must be turned into recognisers
     */
    case ConcatLeft(l, r, t)  => ConcatLeft(transformMap(l, f, u), r, t)
    case ConcatRight(l, r, t) => ConcatRight(l, transformMap(r, f, u), u)

    /**
     * Step 1: find all pattern bindings in the pattern match syntax in `f`
     * Step 2: find all independent applications of each binding, or dead codes thereof
     * Step 3: transfer independent applications to their respective parsers
     * Step 4: rewrite `f` and remove dead code
     * Recursively propagate transform as appropriate
     */
    case Concat(l, r, t) => f match {
      case q"($param) => $body" => body match {

        /** we are assuming only one case in the body */
        case q"$_ match { case $c1 }" => c1 match {

          /**
           * The pattern itself can have two forms:
           *   - just a simple binding, in which case we can't modify anything
           *   - a pair binding, in which case we might salvage from inspecting
           *   the body
           */
          case cq"$pat => $bdy" =>

            /** Get all mappings of a grammar to a binding */
            val mappings: Map[Grammar, Bind] = grammarBindingMappings(g, pat)

            /** Get mappings from each Grammar to a possible Apply tree */
            val indepApplications: Map[Grammar, SymbolUsageInfo] = {
              val bindingSyms = mappings.values.map(_.symbol).toList

              for((grammar, binding) <- mappings) yield {
                val app = largestIndepApplication(
                  binding.symbol,
                  bindingSyms diff List(binding.symbol)
                )(bdy)

                (grammar -> app)
              }
            }

            /**
             * helper functions to identify applications worth manipulating
             */
            def isApp(s: SymbolUsageInfo) = s match {
              case IndepComponent(Apply(_, _)) => true
              case _ => false
            }

            def isAppOrNotUsed(s: SymbolUsageInfo) = s match {
              case IndepComponent(Apply(_, _)) => true
              case NotUsed => true
              case _ => false
            }

            def isNotUsed(s: SymbolUsageInfo) = s match {
              case NotUsed => true
              case _ => false
            }

            /** create, for each Apply, a separate function */
            val newFunctions: Map[Grammar, (Tree, Type)] = for {
              (grammar, indepApp) <- indepApplications if isApp(indepApp)
              binding <- mappings.get(grammar)
            } yield {

              val IndepComponent(app @ Apply(_, _)) = indepApp
              val funArg = freshAnonVal("arg", binding.tpe)
              /** Does the appTransformed *need* to be a `typingTransform`?*/
              val appTransformed = c.internal.typingTransform(app)(
                (tree, api) => tree match {
                  case Ident(_) if tree.symbol == binding.symbol =>
                    api.typecheck(q"${funArg.symbol}")
                  case _ =>
                    api.default(tree)
                }
              )
              val fun = (q"($funArg) => $appTransformed", app.tpe)
              (grammar -> fun)
            }

            /**
             * for each relevant grammar create a Mapped version of it
             * We use Option[Mapped] to denote if a grammar is not used
             * anymore
             */
            val oldAndNew: Map[Grammar, Option[Mapped]] = for {
              (grammar, indepApp) <- indepApplications if isAppOrNotUsed(indepApp)
            } yield {
              val maybeMapped = indepApp match {
                case IndepComponent(Apply(_, _)) =>
                  for ((fun, tpe) <- newFunctions.get(grammar))
                    yield Mapped(grammar, fun, tpe)
                case NotUsed => None
              }
              (grammar -> maybeMapped)
            }

            /**
             * in `bdy`, replace all occurrences of the applications with relevant symbol
             * Since Map uses hashCode and co, and we need ref equality, we'll use
             * a List and find with a predicate
             */
            val reverseMap: List[(Apply, Bind)] = (for {
              (grammar, indepApp) <- indepApplications if isApp(indepApp)
              binding <- mappings.get(grammar)
            } yield {
              val IndepComponent(app @ Apply(_, _)) = indepApp
              (app, binding)
            }).toList

            val newBody = c.internal.typingTransform(bdy)((tree, api) => tree match {
              case app @ Apply(_, _) =>

                reverseMap.find { case (app2, _) => app == app2 } match {
                  case None => api.default(tree)
                  case Some((_, b @ Bind(_, _))) => api.typecheck(q"${b.symbol}")
                }

              case _ => api.default(tree)
            })

            val grammarReworked = swapOldForNew(g, oldAndNew)

            /**
             * rewrite the pattern match in order to remove dead patterns
             */
            val deadBindings: List[Symbol] = (for {
              (grammar, binding) <- mappings
              indepApp <- indepApplications.get(grammar) if isNotUsed(indepApp)
            } yield binding.symbol).toList

            val dcedPattern = eliminateDeadPatterns(pat, deadBindings)

            /**
             * If the new body is just an Ident we don't need it
             */
            val brandNewBody = newBody match {
              case Ident(_) => transform(grammarReworked)
              case _ =>
                val funTransformed = c.internal.typingTransform(f)(
                  (tree, api) =>
                    if (tree == bdy) api.typecheck(newBody)
                    else if (tree == param) {

                      val ValDef(mods, name, tpt, rhs) = param

                      /**
                       * using the internal reflection library to create
                       * a new valDef, with the same symbol as `param`
                       */
                      import c.universe.definitions._
                      import c.internal._, decorators._
                      val sym = param.symbol
                      sym.setInfo(grammarReworked.tpe)
                      val newVal = ValDef(mods, name, q"${grammarReworked.tpe}", rhs)
                      newVal.setSymbol(sym)

                      api.typecheck(newVal)

                    } else if (tree == pat) dcedPattern match {
                      /* nothing in the pattern is used in the body */
                      case EmptyTree => api.typecheck(pq"_")
                      case _ => api.typecheck(dcedPattern)

                    } else api.default(tree)
                )
                Mapped(transform(grammarReworked), funTransformed, u)
            }
            brandNewBody

          case _ => println("matched no pattern"); Mapped(transform(g), f, u)
        }

        case _ =>
          c.warning(f.pos,
            """The body of your function (applying to a concat) could possibly
            benefit from optimisations if you use a pattern match syntax, with
            exactly one case.
            Do consider changing it!
            """
          )
          println(showCode(f))
          println(showRaw(f))
          Mapped(transform(g), f, u)
      }

      case _ =>
        c.warning(f.pos,
          """You are using a function syntax that is not anonymous function
          syntax. As a result you might be forgoing some optimisations we
          could perform. Do consider changing it!
          """
        )
        println(showCode(f))
        println(showRaw(f))
        Mapped(transform(g), f, u)
    }

    case _ => Mapped(transform(g), f, u)
  }

  /**
   * given a grammar and a tree that is a pattern match, returns
   * a mapping of tree to binding. This is used for identifying
   * which parsers are mapped on from a concat:
   *
   * Example: (a ~ b ~ c ~ d) map { case (((p1, p2), p3), p4) => ... }
   * returns (a, p1), (b, p2), (c, p3), (d, p4)
   *
   * Example: (a ~ b ~ c ~ d) map { case ((p1, p3), p4) => ... }
   * returns (a ~ b, p1), (c, p3), (d, p4)
   */
  def grammarBindingMappings(g: Grammar, pat: Tree): Map[Grammar, Bind] = (g, pat) match {
    case (Concat(l, r, _), q"($lpat, $rpat)") =>
      grammarBindingMappings(l, lpat) ++ grammarBindingMappings(r, rpat)
    case (_, b @ Bind(_, _)) => Map(g -> b)
    case _ => Map.empty[Grammar, Bind]
  }

  /**
   * given old to new grammar mappings, yield a new grammar where the old is
   * replaced with the new
   */
  def swapOldForNew(g: Grammar, oldAndNew: Map[Grammar, Option[Mapped]]): Grammar = g match {
    case Concat(l, r, tpe) => oldAndNew.get(g) match {
      case Some(None) => ConcatLeft(l, r, tpe)//TODO: convert into recogniser
      case Some(Some(g2)) => g2
      case _ =>
        val (leftExists, rightExists) = (oldAndNew.get(l), oldAndNew.get(r))
        (leftExists, rightExists) match {
          case (Some(None), Some(Some(_))) =>
            ConcatLeft(swapOldForNew(l, oldAndNew), swapOldForNew(r, oldAndNew), tpe)
          case (Some(Some(_)), Some(None)) =>
            ConcatRight(swapOldForNew(l, oldAndNew), swapOldForNew(r, oldAndNew), tpe)
          case _ => Concat(swapOldForNew(l, oldAndNew), swapOldForNew(r, oldAndNew), tpe)
        }
    }

    case _ => oldAndNew.get(g) match {
      case Some(Some(g2)) => g2
      case Some(None) => g//TODO: convert into recogniser
      case _ => g
    }
  }

  /**
   * Given a pattern-match tree (i.e. `pat` in q"$pat => $body") and a
   * list of dead bindings, returns a new tree devoid of those bindings
   * We naturally
   */
  def eliminateDeadPatterns(pat: c.Tree, deadBindings: List[Symbol]): Tree = {
    if (deadBindings.contains(pat.symbol)) EmptyTree
    else pat match {
      case q"($lpat, $rpat)" =>
        (eliminateDeadPatterns(lpat, deadBindings), eliminateDeadPatterns(rpat, deadBindings)) match {
          case (EmptyTree, EmptyTree) => EmptyTree
          case (EmptyTree, r) => r
          case (l, EmptyTree) => l
          case (l, r) => q"($l, $r)"
        }
      case _ => pat
    }
  }
}
