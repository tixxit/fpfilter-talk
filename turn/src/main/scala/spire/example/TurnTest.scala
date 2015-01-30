package spire.example

import scala.{ specialized => spec }
import scala.annotation.tailrec
import scala.reflect.ClassTag

import spire.algebra._
import spire.math._
import spire.random._
import spire.implicits._

object TurnTest extends App {
  case class Simplex(
    px: Double, py: Double,
    qx: Double, qy: Double,
    rx: Double, ry: Double
  ) {
    def orientation(implicit turn: Turn[Double]): Sign =
      turn(px, py, qx, qy, rx, ry)
  }

  def degree: Dist[Double] =
    Dist.uniform(0, 2 * scala.math.Pi)

  def jitter(x: Double, y: Double, eps: Double): Dist[(Double, Double)] = for {
    theta <- degree
    mag   <- Dist.uniform(0, eps)
    dx     = cos(theta) * mag
    dy     = sin(theta) * mag
  } yield (x + dx, x + dy)

  def simplex(eps: Double): Dist[Simplex] = for {
    ox    <- Dist.double
    oy    <- Dist.double
    pmag  <- Dist.double
    qmag  <- Dist.double
    rmag  <- Dist.double
    theta <- degree
    dx     = cos(theta)
    dy     = sin(theta)
    (px, py) <- jitter(ox + pmag * dx, oy + pmag * dy, eps)
    (qx, qy) <- jitter(ox + qmag * dx, oy + qmag * dy, eps)
    (rx, ry) <- jitter(ox + rmag * dx, oy + rmag * dy, eps)
  } yield Simplex(px, py, qx, qy, rx, ry)

  def test(eps: Double): Dist[Boolean] = for {
    s <- simplex(eps)
  } yield {
    Sign(s.orientation(FastTurn)) == Sign(s.orientation(ExactTurn))
  }

  println(s"Epsilon,Accuracy")
  (0 to 56) map { k =>
    val eps = 1D / pow(2D, k)
    val sample = test(eps).sample[Vector](10000)
    val accuracy = sample.filter(_ == true).size.toDouble / sample.size.toDouble
    println(s"$k,$accuracy")
  }
}
