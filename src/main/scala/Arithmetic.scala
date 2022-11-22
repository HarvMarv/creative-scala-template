// A simple arithmetic expression is:
import scala.annotation.tailrec

// Add variable to expression data type
// Calculate of derive(f, x) = f(Dual(x)).ϵ - https://nextjournal.com/erik-engheim/implementation-of-automatic-differentiation
// evalDual needs to accept point at which you evaulate
// when encountering the variable we have the dual number with real component = the point at which we're evalating the function and epsilon = 1

// EG 1
// y = x * x where x = 4
// Replace `x` with Dual(x, 1) and any `Literal` with Dual(LiteralValue, 0)
// => Dual(4, 1) * Dual(4, 1)
// Dual(16, 8) => gradient == 8 when x=4

// EG 2
// y = x + 2 where x = 5
// => Dual(5, 1) + Dual(2, 0)
// = Dual(7, 1) => gradient == 1 when x=5

object Arithmetic {

  sealed trait Expression {
    def eval: (Expression) => Double = {
      this match {
        case Literal(literal) => (_) => literal
        case Addition(left, right) => (x: Expression) => left.eval(x) + right.eval(x)
        case Subtraction(left, right) => (x: Expression) => left.eval(x) - right.eval(x)
        case Multiplication(left, right) => (x: Expression) => left.eval(x) * right.eval(x)
        case Division(left, right) => (x: Expression) => left.eval(x) / right.eval(x)
        // case Power(base, power) => (x: Expression) => Math.pow(base.eval(x), power.eval(x))
        case Sin(theta) => (x: Expression) => Math.sin(theta.eval(x))
        case Cos(theta) => (x: Expression) => Math.cos(theta.eval(x))
        case Tan(theta) => (x: Expression) => Math.tan(theta.eval(x))
        case Var() => (x: Expression) => x.eval(x)
      }
    }
    def differentiateAboutThePoint(x: Double): Double = {
      this.evalDual(x).b
    }
    override def toString: String = {
      this match {
        // Base
        case Literal(literal) => s"${literal}"
        // Arithmetic
        case Addition(left, right) =>
          s"($left + $right)"
        case Subtraction(left, right) =>
          s"($left - $right)"
        case Multiplication(left, right) =>
          s"($left * $right)"
        case Division(left, right) =>
          s"($left / $right)"
        // case Power(base, power) =>
        //   s"($base)^($power)"
        // Trig
        case Sin(theta) => s"sin($theta)"
        case Cos(theta) => s"cos($theta)"
        case Tan(theta) => s"tan($theta)"
        case Var() => s"𝔁"
      }
    }
    def evalDual(x: Double): Dual = {
      this match {
        case Var() => Dual(x, 1)
        case Literal(value) => Dual(value)
        case Addition(l, r) => l.evalDual(x) + r.evalDual(x)
        case Subtraction(l, r) => l.evalDual(x) - r.evalDual(x)
        case Multiplication(l, r) => l.evalDual(x) * r.evalDual(x)
        // case Power(base, power) => base.evalDual(x) * Power(base, Subtraction(power, Literal(1.0))).evalDual(x)
        case Division(l, r) => l.evalDual(x) / r.evalDual(x)
        case Sin(theta) => {
          val dual = theta.evalDual(x)
          Dual(Math.sin(dual.a), dual.b * Math.cos(dual.a))
        }
        case Cos(theta) => {
          val dual = theta.evalDual(x)
          Dual(Math.cos(dual.a), -dual.b * Math.sin(dual.a))
        }
        case Tan(theta) => {
          val dual = theta.evalDual(x)
          Dual(Math.tan(dual.a), dual.b / (Math.cos(dual.a) * Math.cos(dual.a)))
        }
      }
    }
    // def toDual = Dual(this.eval(Dual(x, 1)))
  }
  // Base
  final case class Literal(literal: Double) extends Expression
  // Arithmetic
  final case class Addition(left: Expression, right: Expression)
    extends Expression
  final case class Subtraction(left: Expression, right: Expression)
    extends Expression
  final case class Multiplication(
    left: Expression,
    right: Expression
  ) extends Expression
  final case class Division(left: Expression, right: Expression)
    extends Expression
  // final case class Power(base: Expression, power: Expression) extends Expression
  // Trigonometry
  final case class Sin(theta: Expression) extends Expression
  final case class Cos(theta: Expression) extends Expression
  final case class Tan(theta: Expression) extends Expression
  // Var
  final case class Var() extends Expression

  final case class Dual(a: Double, b: Double = 0) {
    def +(that: Dual): Dual =
      Dual(a + that.a, b + that.b)

    def -(that: Dual): Dual =
      Dual(a - that.a, b - that.b)

    def *(that: Dual): Dual =
      Dual(a * that.a, a * that.b + b * that.a)

    def /(that: Dual): Dual =
      Dual(a / that.a, (b * that.a - a * that.b) / that.a * that.a)

  }

}
