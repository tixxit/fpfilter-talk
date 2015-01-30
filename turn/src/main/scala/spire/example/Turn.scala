package spire.example

import scala.{ specialized => spec }

import spire.algebra._
import spire.math._
import spire.random._
import spire.implicits._

trait Turn[@spec A] {
  def apply(
    px: A, py: A,
    qx: A, qy: A,
    rx: A, ry: A
  ): Int
}

object FastTurn extends Turn[Double] {
  def apply(
    px: Double, py: Double,
    qx: Double, qy: Double,
    rx: Double, ry: Double
  ): Int =
    spire.math.signum((qx - px) * (ry - py) - (rx - px) * (qy - py))
}

object ExactTurn extends Turn[Double] {
  def apply(
    px: Double, py: Double,
    qx: Double, qy: Double,
    rx: Double, ry: Double
  ): Int = {
    val pxa = Algebraic(px)
    val pya = Algebraic(py)
    val qxa = Algebraic(qx)
    val qya = Algebraic(qy)
    val rxa = Algebraic(rx)
    val rya = Algebraic(ry)
    ((qxa - pxa) * (rya - pya) - (rxa - pxa) * (qya - pya)).signum
  }
}

object FilteredTurn extends Turn[Double] {
  def apply(
    px: Double, py: Double,
    qx: Double, qy: Double,
    rx: Double, ry: Double
  ): Int = {
    val pxf = FpFilter.exact[Algebraic](px)
    val pyf = FpFilter.exact[Algebraic](py)
    val qxf = FpFilter.exact[Algebraic](qx)
    val qyf = FpFilter.exact[Algebraic](qy)
    val rxf = FpFilter.exact[Algebraic](rx)
    val ryf = FpFilter.exact[Algebraic](ry)
    ((qxf - pxf) * (ryf - pyf) - (rxf - pxf) * (qyf - pyf)).signum
  }
}
