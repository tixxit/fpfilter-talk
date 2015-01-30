package spire.example

import scala.reflect.ClassTag

import spire.algebra._
import spire.math._
import spire.random._
import spire.implicits._

object PolyTest extends App {
  def test[A: Ring: IsAlgebraic: Uniform: ClassTag](degree: Dist[Int], root: Dist[A], eps: A): Dist[Boolean] = {
    val fastTest = new FastSignTest[A]
    val exactTest = new ExactSignTest[A]

    for {
      d <- degree
      roots <- Dist.list(d, d)(root)
      poly = roots.foldLeft(Polynomial.one[A]) { (acc, x) =>
        acc * Polynomial.linear(Rig[A].one, -x)
      }
      r <- Dist.oneOf(roots: _*)
      x <- Dist.uniform(r - eps, r + eps)
    } yield {
      fastTest(poly, x) == exactTest(poly, x)
    }
  }

  val degrees = List(8, 16, 32, 64)
  println(s"Epsilon,${degrees.mkString(",")}")
  (1 to 64) map { k =>
    val degree = Dist.uniform(1, 16)
    val root = Dist.uniform(BigDecimal(-1), BigDecimal(1))
    val eps = BigDecimal(1, k)
    val cells = k.toString :: degrees.map { d =>
      val degree = Dist.uniform(1, d)
      val sample = test(degree, root, eps).sample[List](1000)
      val accuracy = sample.filter(_ == true).size.toDouble / sample.size.toDouble
      accuracy.toString
    }
    println(cells.mkString(","))
  }
}
