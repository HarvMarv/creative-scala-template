import doodle.core._
import doodle.image._
import doodle.image.syntax.all._
import doodle.image.syntax.core._
import doodle.java2d._
import doodle.reactor._

import scala.concurrent.duration._
import cats.effect.unsafe.implicits.global
import org.apache.bcel.generic.ArrayType

import scala.util.Random
import scala.collection.mutable.ListBuffer
import java.time.ZonedDateTime

object MachineLearning extends App {

  object Differentiator {
    //                            step       f(x) could be x^2   => func which we give X, and returns the gradient at X
    def numericalDifferentiation(h: Double)(f: Double => Double): Double => Double = {
      x: Double => (f(x + h) - f(x)) / h
    }

    def pointLoss: ((Double, Double) => Double, Double, Double, Double) => Double =
      (f, a, x, y) => {
        val error = f(x, a) - y
        error * error
      }

    def loss: List[Point] => ((Double, Double) => Double) => Double => Double =
      (data) =>
        (f: (Double, Double) => Double) =>
          (a: Double) =>
            data.foldLeft(0.0){ (accum, pt) => pointLoss(f, a, pt.x, pt.y) }
  }

}
