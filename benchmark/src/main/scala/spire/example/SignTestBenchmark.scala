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
class SignTestBenchmark {
  @Param(Array("8", "16", "32", "64"))
  var degree: Int = _

  @Param(Array("2", "4", "8", "12", "16", "20", "24", "28", "32"))
  var epsExp: Int = _

  def eps: BigDecimal = BigDecimal(1, epsExp)

  // In our root finding algorithm, we will always be testing signs in an
  // isolating interval, so we want to try and replicate that here.  We
  // distribute the roots uniformly within [-degree, degree]. This just ensures
  // that the roots are generally well separated, so that we aren't getting
  // messed up too much by testing the signs of nearby roots, rather than the
  // root we're intrested in. A better benchmark may ensure that all the roots
  // are always isolated for each test, but I think this is good enough.
  def root = Dist.uniform(BigDecimal(-degree), BigDecimal(degree))

  def testDist: Dist[(Polynomial[BigDecimal], BigDecimal)] = for {
    roots <- Dist.list(degree, degree)(root)
    poly = roots.foldLeft(Polynomial.one[BigDecimal]) { (acc, x) =>
      acc * Polynomial.linear(BigDecimal(1), -x)
    }
    r <- Dist.oneOf(roots: _*)
    x <- Dist.uniform(r - eps, r + eps)
  } yield {
    poly -> x
  }

  var tests: Vector[(Polynomial[BigDecimal], BigDecimal)] = _

  @Setup
  def setup(): Unit = {
    tests = testDist.sample[Vector](100)
  }

  val fastSignTest = new FastSignTest[BigDecimal]
  val exactSignTest = new ExactSignTest[BigDecimal]
  val fpFilterSignTest = new FpFilterSignTest[BigDecimal]

  @Benchmark
  def testFastSignTest(): Vector[Sign] =
    tests.map(fastSignTest.tupled)

  @Benchmark
  def testExactSignTest(): Vector[Sign] =
    tests.map(exactSignTest.tupled)

  @Benchmark
  def testFpFilterSignTest(): Vector[Sign] =
    tests.map(fpFilterSignTest.tupled)
}
