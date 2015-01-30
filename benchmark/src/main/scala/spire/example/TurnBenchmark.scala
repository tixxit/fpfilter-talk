package spire.example

import spire.algebra._
import spire.math._
import spire.random._
import spire.implicits._

import org.openjdk.jmh.annotations.Benchmark
import org.openjdk.jmh.annotations.Param
import org.openjdk.jmh.annotations.Scope
import org.openjdk.jmh.annotations.Setup
import org.openjdk.jmh.annotations.State
import org.openjdk.jmh.annotations.{ Fork, Warmup, Measurement }

@Warmup(iterations = 5)
@Measurement(iterations = 10)
@Fork(1)
@State(Scope.Benchmark)
class TurnBenchmark {

  @Param(Array("0", "8", "16", "24", "32", "40", "48", "56"))
  var epsExp: Int = _

  def eps: Double = 1D / spire.math.pow(2D, epsExp)

  var tests: Array[Double] = _

  @Setup
  def setup(): Unit = {
    val sample = TurnTest.simplex(eps).sample[Vector](1000)
    tests = sample.toArray.flatMap { case TurnTest.Simplex(px, py, qx, qy, rx, ry) =>
      Array(px, py, qx, qy, rx, ry)
    }
  }

  //@Benchmark
  //def testFastTurn(): Int = {
  //  var i = 0
  //  var sum = 0
  //  while (i < tests.length) {
  //    val px = tests(i)
  //    val py = tests(i + 1)
  //    val qx = tests(i + 2)
  //    val qy = tests(i + 3)
  //    val rx = tests(i + 4)
  //    val ry = tests(i + 5)
  //    sum += FastTurn(px, py, qx, qy, rx, ry)
  //    i += 6
  //  }
  //  sum
  //}

  //@Benchmark
  //def testFilteredTurn(): Int = {
  //  var i = 0
  //  var sum = 0
  //  while (i < tests.length) {
  //    val px = tests(i)
  //    val py = tests(i + 1)
  //    val qx = tests(i + 2)
  //    val qy = tests(i + 3)
  //    val rx = tests(i + 4)
  //    val ry = tests(i + 5)
  //    sum += FilteredTurn(px, py, qx, qy, rx, ry)
  //    i += 6
  //  }
  //  sum
  //}

  @Benchmark
  def testExactTurn(): Int = {
    var i = 0
    var sum = 0
    while (i < tests.length) {
      val px = tests(i)
      val py = tests(i + 1)
      val qx = tests(i + 2)
      val qy = tests(i + 3)
      val rx = tests(i + 4)
      val ry = tests(i + 5)
      sum += ExactTurn(px, py, qx, qy, rx, ry)
      i += 6
    }
    sum
  }
}
