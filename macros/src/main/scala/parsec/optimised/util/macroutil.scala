package macroutil {

  import scala.reflect.macros.blackbox.Context

  case class OrigOwnerAttachment(sym: Any)

  object Splicer {
    import scala.reflect.macros._
    import blackbox.Context
    def impl[A](c: Context)(expr: c.Expr[A]): c.Expr[A] = {
      val helper = new Splicer[c.type](c)
      c.Expr[A](helper.changeOwner(expr.tree))
    }

    class Splicer[C <: Context](val c: C) {
      import c.universe._
      import c.universe.definitions._
      import c.internal._, decorators._

      def changeOwner(tree: c.Tree): c.Tree = {
        import c.universe._, internal._, decorators._
        val origOwner = tree.attachments.get[OrigOwnerAttachment].map(_.sym).get.asInstanceOf[Symbol]
        c.internal.changeOwner(tree, origOwner, c.internal.enclosingOwner)
      }
    }
    def changeOwner[A](expr: A): A = macro impl[A]
  }
}
