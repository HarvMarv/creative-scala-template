import doodle.core._
import doodle.image._
import doodle.image.syntax.all._
import doodle.image.syntax.core._
import doodle.java2d._
import doodle.reactor._
import scala.concurrent.duration._
import cats.effect.unsafe.implicits.global
import scala.util.Random
import scala.collection.mutable.ListBuffer

// To use this example:
//
// 1. run `sbt`
// 2. run the `run` command within `sbt`

object Animation extends App {

  def newStarList(n: Int): List[Int => Image] = {
    val list = new ListBuffer[Int => Image]()
    val rng = scala.util.Random

    for (i <- 0 to n)
      list += newStar(rng)

    list.toList
  }

  def newStar(rng: Random): Int => Image = {
    var x_var = rng.nextInt(100) + 50
    var y_var = rng.nextInt(100) + 50

    var speed = (rng.nextInt(5000) + 1000) / 100

    val points = rng.nextInt(20) + 5
    var outerRadius = rng.nextInt(70) + 10
    var innerRadius = rng.nextInt(50) + 10
    // swap radii if inner > outer
    var radiusPair = (outerRadius, innerRadius)

    if (outerRadius > innerRadius) radiusPair = radiusPair.swap

    (x: Int) =>
      Image
        .star(points, radiusPair._1, radiusPair._2)
        .noStroke
        .fillColor(Color.slateGray)
        .at((x * speed).degrees.cos * x_var, (x * speed).degrees.sin * y_var)

  }

  val image =
    Image
      .circle(100)
      .fillColor(Color.red)
      .on(Image.circle(200).fillColor(Color.aquamarine))
      .on(Image.circle(300).fillColor(Color.steelBlue))

  val animation = {
    val stars = newStarList(5)
    var change = 1;
    Reactor
      .init(-200)
      .withOnTick(x => {
        // if (x == 200) change = -1 else if (x == -200) change = 1
        x + change
      })
      .withTickRate(5.millis)
      .withRender { x =>
        val y = x.degrees.cos * 200
        val new_x = x.degrees.sin * 100
        val planet = Image.circle(50.0).noStroke.fillColor(Color.crimson)

        var combo = planet.at(new_x, y)
        for (star <- stars) combo = star(x).at(new_x, y).on(combo)

        combo
      }
  }
  val a = Option("a")

  println(a.map(_ + "b"))

  val b = Option("b")
  println(b.flatMap(f => Option(f + "c")))

  val frame = Frame.size(600, 600)

  animation.run(frame)
}
