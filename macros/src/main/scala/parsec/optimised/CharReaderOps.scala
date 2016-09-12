package parsec.optimised

import scala.reflect.macros.blackbox.Context
import util.Zeroval

trait ReaderOps {

  val c: Context
  import c.universe._

  abstract class Reader(elemType: Type) {
    def first: Tree
    def atEnd: Tree
    def rest: Reader
  }

  /**
   * a CPS-encoded implementation of the CharReader datatype
   *
   * for reminder sake:
   *   CharReader =
   *     forall X. ((source: Array[Char], pos: Int) => X) => X
   */
  abstract class CharReader extends Reader(typeOf[Char]) { self =>
    def apply(cont: (Tree, Tree) => Tree): Tree

    def first = self.apply((source, pos) => q"$source($pos)")
    def atEnd = self.apply((source, pos) => q"$pos >= $source.length")
    def rest = new CharReader {
      def apply(cont: (Tree, Tree) => Tree) =
        self.apply((source, pos) => cont(source, q"$pos + 1"))
    }

    def getSource = self.apply((src, _) => src)
    def getPos = self.apply((_, p) => p)

    def toCharReader: Tree = self.apply {
      //(source, pos) => q"CharReader(${source.symbol}, ${pos.symbol})"
      (source, pos) => q"CharReader($source, $pos)"
    }
  }

  def mkCharReader(source: Tree, pos: Tree) = new CharReader {
    def apply(cont: (Tree, Tree) => Tree) = cont(source, pos)
  }

  def cond(test: Tree, thenp: CharReader, elsep: CharReader) = {
    new CharReader {
      def apply(cont: (Tree, Tree) => Tree) = {
        q"""
          if ($test) ${thenp(cont)}
          else       ${elsep(cont)}
        """
      }
    }
  }

}
