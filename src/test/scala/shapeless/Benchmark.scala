import scala.tools.nsc._

import org.scalameter.api._
import org.scalameter.picklers.Implicits._

object Benchmark extends Bench.ForkedTime {

  override def persistor = Persistor.None
  override def reporter = new LoggingReporter

  val range = Gen.enumeration("size")(10)

  performance of "Compilation of shapeless" in {
    measure method "scala-compiler" config (
      exec.minWarmupRuns -> 5,
      exec.maxWarmupRuns -> 10,
      exec.benchRuns -> 15,
      exec.independentSamples -> 4
    ) in {
        using(range) in { n =>

          /* Let's create a new scala compiler! */
          val s = new Settings
          s.usejavacp.value = true
          val compiler = new Global(s)
          val run = new compiler.Run()

          run.compile(List("src/test/scala/shapeless/Shapeless.scala"))

        }
      }
  }

}
