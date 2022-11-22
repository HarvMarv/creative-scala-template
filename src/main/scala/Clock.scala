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

// To use this example:
//
// 1. run `sbt`
// 2. run the `run` command within `sbt`
object Clock extends App {
  case class Colon() extends Displayable
  case class Zero() extends Displayable
  case class One() extends Displayable
  case class Two() extends Displayable
  case class Three() extends Displayable
  case class Four() extends Displayable
  case class Five() extends Displayable
  case class Six() extends Displayable
  case class Seven() extends Displayable
  case class Eight() extends Displayable
  case class Nine() extends Displayable

  trait Displayable {
    def getDisplayArray(): Array[Array[Boolean]] = {
      val matrix = Array.ofDim[Boolean](6, 3)
      for (i <- 0 until matrix(0).length - 1; j <- 0 until matrix.length - 1) {
        this match {
          case Colon() => matrix(j)(i) = if (j == 1 && (i == 1 || i == 4)) true else false
          case Zero() => matrix(j)(i) = if (j != 1 || i != 1) true else false
          case One() => matrix(j)(i) = if (j == 1 || (j == 1 && i == 1) || j == 5) true else false
          case Two() => ???
          case Three() => ???
          case Four() => ???
          case Five() => ???
          case Six() => ???
          case Seven() => ???
          case Eight() => ???
          case Nine() => ???
        }
      }
      matrix
    }

    override def toString(): String = {
      val matrix = this.getDisplayArray()
      var accum = ""
      for (i <- 0 to matrix.length) {
        for (j <- 0 to matrix(0).length) {
          // Accessing the values
          accum += { if (matrix(j)(i)) "XX" else "  " }
        }
        accum += "\n\r"
      }
      // this.getDisplayArray().map(a => accum += a.map(b => "" += {if (b) "XX" else "  "}) + "\n")
      accum
    }
  }

  // val animation = {
  //   var change = 1;
  //   Reactor
  //     .init(ZonedDateTime.now())
  //     .withOnTick(x => {
  //       ZonedDateTime.now()
  //     })
  //     .withTickRate(1.second)
  //     .withRender { x =>
  //   val clock = Clock()
  //       val y = x.degrees.cos * 200
  //       val new_x = x.degrees.sin * 100
  //       // val planet = Image.(50.0).noStroke.fillColor(Color.crimson)

  //       var combo = planet.at(new_x, y)
  //       for (star <- stars) combo = star(x).at(new_x, y).on(combo)

  //       combo
  //     }
  // }
  // val a = Option("a")

  // println(a.map(_ + "b"))

  // val b = Option("b")
  // println(b.flatMap(f => Option(f + "c")))

  // val frame = Frame.size(600, 600)

  // animation.run(frame)

  final case class Clock() {
    def getTime(): ZonedDateTime = {
      ZonedDateTime.now()
    }
  }
}
