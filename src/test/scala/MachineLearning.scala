import MachineLearning._

import java.time.ZonedDateTime

class MachineLearningSuite extends munit.FunSuite {

  test("differentiate") {
    val tool = Differentiator
    val f = (x: Double) => x * x
    def square(x: Double) = x * x
    val h = 0.00001

    val xSquaredGradientAt: Double => Double = tool.numericalDifferentiation(h)(f)
    val gradient = xSquaredGradientAt(1.0)

    assertEqualsDouble(gradient, 2.0, 0.0001)
  }
}
