scalaVersion := "2.13.8"

console / initialCommands := """
      |import doodle.core._
      |import doodle.image._
      |import doodle.image.syntax.all._
      |import doodle.image.syntax.core._
      |import doodle.java2d._
      |import cats.effect.unsafe.implicits.global
    """.trim.stripMargin

libraryDependencies ++= Seq(
  "org.creativescala" %% "doodle" % "0.11.2",
  "org.scalameta" %% "munit" % "0.7.29" % Test,
  "com.softwaremill.magnolia1_2" %% "magnolia" % "1.1.2"
)
