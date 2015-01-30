package spire.example

import scala.{ specialized => spec }
import scala.annotation.tailrec

import spire.algebra._
import spire.math._
import spire.random._
import spire.implicits._

trait SignTest[A] extends ((Polynomial[A], A) => Sign) {
  def apply(poly: Polynomial[A], x: A): Sign
}

final class FastSignTest[@spec(Double) A: Semiring: Eq](implicit A: IsReal[A]) extends SignTest[A] {
  def apply(poly: Polynomial[A], x: A): Sign = {
    val x0 = A.toDouble(x)

    @tailrec
    def loop(acc: Double, i: Int): Sign = if (i >= 0) {
      loop(A.toDouble(poly.nth(i)) + acc * x0, i - 1)
    } else {
      acc.sign
    }

    loop(0, poly.degree)
  }
}

final class ExactSignTest[@spec(Double) A: Semiring: Eq](implicit A: IsAlgebraic[A]) extends SignTest[A] {
  def apply(poly: Polynomial[A], x: A): Sign = {
    val x0 = A.toAlgebraic(x)

    @tailrec
    def loop(acc: Algebraic, i: Int): Sign = if (i >= 0) {
      loop(A.toAlgebraic(poly.nth(i)) + acc * x0, i - 1)
    } else {
      acc.sign
    }

    loop(Algebraic.Zero, poly.degree)
  }
}

final class FpFilterSignTest[@spec(Double) A: Semiring](implicit A: IsAlgebraic[A]) extends SignTest[A] {
  def apply(poly: Polynomial[A], x: A): Sign = {
    val x0 = FpFilter(A.toDouble(x), A.toAlgebraic(x))

    @tailrec
    def loop(acc: FpFilter[Algebraic], i: Int): Sign = if (i >= 0) {
      val c = poly.nth(i)
      loop(FpFilter(A.toDouble(c), A.toAlgebraic(c)) + acc * x0, i - 1)
    } else {
      Sign(acc.signum)
    }

    loop(FpFilter.approx(Algebraic.Zero), poly.degree)
  }
}

