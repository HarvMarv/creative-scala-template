import Arithmetic._
import org.junit.experimental.categories.Category

class Arithmetics extends munit.Tag("Arithmetics")
class Expressions extends munit.Tag("Expressions")
class Duals extends munit.Tag("Duals")

@Category(Array(classOf[Arithmetics], classOf[Expressions]))
class ArithmeticsSuite extends munit.FunSuite {
  val one = Literal(1.0)
  val two = Literal(2.0)

  // Basics
  test("Literal") {
    val expr = Literal(1.0).eval(Literal(1))
    val delta = 0.001
    assertEqualsDouble(expr, 1.0, delta)
  }

  // test("Var") {
  //   val expr = Var().eval
  //   val delta = 0.001
  //   assertEqualsDouble(expr, 1.0, delta)
  // }

  test("Addition") {
    val expr = Addition(one, Literal(1.0)).eval(Literal(1))
    val delta = 0.001
    assertEqualsDouble(expr, 2.0, delta)
  }

  test("Subtraction") {
    val expr = Subtraction(two, one).eval(Literal(1))
    val delta = 0.001
    assertEqualsDouble(expr, 1.0, delta)
  }

  test("Multiplication") {
    val expr = Multiplication(one, two).eval(Literal(1))
    val delta = 0.001
    assertEqualsDouble(expr, 2.0, delta)
  }

  test("Division") {
    val expr = Division(two, two).eval(Literal(1))
    val delta = 0.001
    assertEqualsDouble(expr, 1.0, delta)
  }

  test("Sin") {
    val expr = Sin(two).eval(Literal(1))
    val delta = 0.001
    assertEqualsDouble(expr, Math.sin(2.0), delta)
  }

  test("Cos") {
    val expr = Cos(two).eval(Literal(5))
    val delta = 0.001
    assertEqualsDouble(expr, Math.cos(2.0), delta)
  }

  test("Tan") {
    val expr = Tan(two).eval(Literal(5))
    val delta = 0.001
    assertEqualsDouble(expr, Math.tan(2.0), delta)
  }

  test("repr") {
    val expr = Tan(two)
    assertEquals(expr.toString, "tan(2.0)")
  }

  test("Var()") {
    val fiveSqrd = Multiplication(Var(), Var()).eval(Literal(5.0))
    assertEquals(fiveSqrd, 25.0)
  }

  test("differentiate x^2 WRT x") {
    val y = Addition(Multiplication(Var(), Var()), Addition(Multiplication(Literal(5.0), Var()), Literal(2)))
    for (x <- -10 to 10) {
      val gradient = y.differentiateAboutThePoint(x)
      println(s"gradient of x^2 + 5x + 2 at x=$x is: $gradient ")

      // We know that f'(x) = 2x
      // So lets test it
      assertEquals(gradient, (x * 2.0 + 5))
    }
  }
  test("differentiate sin(x) WRT x") {
    val y = Sin(Var())
    for (x <- -360 to 360) {
      val gradient = y.differentiateAboutThePoint(x)
      assertEquals(gradient, (Math.cos(x)))
    }
  }
  test("differentiate cos(x) WRT x") {
    val y = Cos(Var())
    for (x <- -360 to 360) {
      val gradient = y.differentiateAboutThePoint(x)
      assertEquals(gradient, (-Math.sin(x)))
    }
  }
  test("differentiate tan(x) WRT x") {
    val y = Tan(Var())
    for (x <- -360 to 360) {
      val gradient = y.differentiateAboutThePoint(x)
      assertEqualsDouble(gradient, (1 + (Math.tan(x) * Math.tan(x))), 0.01)
    }
  }
  test("differentiate polynomial WRT x") {
    val y = Addition(
      Multiplication(Var(), Multiplication(Var(), Multiplication(Var(), Var()))),
      Addition(
        Multiplication(Literal(2), Multiplication(Var(), Multiplication(Var(), Var()))),
        Var()
      )
    )
    for (x <- -360 to 360) {
      val gradient = y.differentiateAboutThePoint(x)
      assertEqualsDouble(gradient, (Math.pow(x, 3) * 4) + (Math.pow(x, 2) * 3 * 2) + 1, 0.01)
    }
  }
}

@Category(Array(classOf[Arithmetics], classOf[Duals]))
class DualsSuite extends munit.FunSuite {
  test("expr") {
    val fiveSqrd = Multiplication(Var(), Var()).eval(Literal(5.0))
    assertEquals(fiveSqrd, 25.0)
  }

  test("evalDual") {
    val eight = Multiplication(Var(), Var()).evalDual(4.0).b
    assertEquals(eight, 8.0)
  }
}
