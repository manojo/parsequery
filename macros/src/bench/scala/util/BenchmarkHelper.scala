package util

import org.scalameter.api._
import org.scalameter.picklers.Implicits._
import org.scalameter.Measurer.{
  RelativeNoise,
  OutlierElimination,
  PeriodicReinstantiation,
  MemoryFootprint
}
import org.scalameter.Key

import parsec._
import parsec.optimised._

import java.text.SimpleDateFormat
import java.util.Calendar

trait BenchmarkHelper extends Bench.ForkedTime with java.io.Serializable {

  override def executor = SeparateJvmsExecutor(
    new Executor.Warmer.Default,
    Aggregator.min[Double],
    new Measurer.Default
  )

  override def persistor = Persistor.None
  override def reporter = new LoggingReporter

  def independentSamples = 1
  def benchRunsPerSample = 10
  def benchRuns = independentSamples * benchRunsPerSample

  def memoryInHeap = "2g"

  val dateTimeFormat = new SimpleDateFormat("yyyy-MM-dd HH:mm")
  val dateTime = dateTimeFormat.format(Calendar.getInstance().getTime)

  final val home = sys.env.apply("HOME")
  final val yourkitPath =
    s"/home/manojo/software/yourkit/bin/linux-x86-64/libyjpagent.so"

  def performanceOfParsers[T](measurer: Gen[T] => Unit)(implicit seed: Gen[T]): Unit = {
    performance of "parsers" config(
      Key.exec.benchRuns -> benchRuns,
      Key.verbose -> true,
      Key.exec.independentSamples -> independentSamples,
      // Key.reports.resultDir -> "benchmark_results"
      Key.exec.jvmflags -> List(
        s"-Xms$memoryInHeap",
        s"-Xmx$memoryInHeap",
        s"-agentpath:$yourkitPath"
        // "-XX:+UnlockDiagnosticVMOptions",
        // "-XX:+PrintInlining",
        // "-XX:+PrintCompilation"
      ),
      Key.reports.resultDir -> s"report-$dateTime"
    ) in { measurer(seed) }
  }

}

trait ParsequeryBenchmarkHelper extends BenchmarkHelper with OptimisedParsers {

  def runBM(g: Gen[List[String]], mName: String, meth: Parser[_]): Unit = {
    measure method mName in {
      using(g) in { fs =>
        for (f <- fs) {
          val fileName = "data/" + f
          val file = scala.io.Source.fromFile(fileName).mkString
          val fileArray = file.toCharArray
          performance of s"$mName benchmark" in {
            val Success(res, _) = meth(CharReader(fileArray))
          }
        }
      }
    }
  }
}

trait FastParseBenchmarkHelper extends BenchmarkHelper {
  import fastparse.all._

  def runBM(g: Gen[List[String]], mName: String, meth: P[_]): Unit = {
    measure method mName in {
      using(g) in { fs =>
        for (f <- fs) {
          val fileName = "data/" + f
          val file = scala.io.Source.fromFile(fileName).mkString
          performance of s"$mName benchmark" in {
            val Parsed.Success(res, _) = meth.parse(file)
          }
        }
      }
    }
  }
}
