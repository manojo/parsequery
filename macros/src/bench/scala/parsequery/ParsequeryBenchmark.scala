/*package parsequery

import org.scalameter.api._
import org.scalameter.picklers.Implicits._
import org.scalameter.Measurer.{RelativeNoise, OutlierElimination, PeriodicReinstantiation, MemoryFootprint}
import org.scalameter.Key

import java.text.SimpleDateFormat
import java.util.Calendar

import parsec._
import parsec.optimised._

object ParsequeryBenchmark extends Bench.ForkedTime with JSONParser {

  import Js._
  import fastparse.all._

  override def executor = SeparateJvmsExecutor(
    new Executor.Warmer.Default,
    Aggregator.min[Double],
    new Measurer.Default
  )

  override def persistor = Persistor.None
  override def reporter = new LoggingReporter

  def benchmark(fname: String, f: String => _): Unit = {
    import scala.io.Source
    val fileName = "data/scala-lang-contributions.json"
    val fileContent = Source.fromFile(fileName).mkString

    val range = Gen.enumeration("size")(10)
    performance of fname in {
      measure method "parser" config (
        exec.minWarmupRuns -> 5,
        exec.maxWarmupRuns -> 10,
        exec.benchRuns -> 25,
        exec.independentSamples -> 1
      ) in {
          using(range) in { n =>
            // we use while to remove overhead of for ... yield
            var i = 0
            while (i < n) {
              f(fileContent)
              i += 1
            }
          }
        }
    }
  }

  def runGeneralParser(src: String) = {
    val Parsed.Success(resAll, _) = jsonExpr.parse(src)
    val ids2totals: List[(Val, Val)] = (resAll match {
      case x @ Arr(ls) =>
        for (l <- ls) yield (l("author")("id"), l("total"))
      case _ => sys.error("Something went wrong")
    }).toList
  }

  def runSpecializedParser(src: String) = {
    val Parsed.Success(specialized, _) = projections.parse(src)
    val ids2totalsBis: List[Val] = (specialized match {
      case x @ Arr(ls) => ls
    }).toList
  }

  benchmark("GeneralParser", runGeneralParser)
  benchmark("SpecializedParser", runSpecializedParser)
}
*/
