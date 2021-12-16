package code

import org.scalatest._
import matchers.should._
import org.scalatest.funsuite.AnyFunSuite

sealed trait Result[T]
final case class Success[T](value: T) extends Result[T]
final case class Failure[T](reason: String) extends Result[T]


sealed trait Expression {

  def eval: Result[Double] = this match {
    case Addition(l, r) =>
      l.eval match {
        case Failure(reason) => Failure(reason)
        case Success(lvalue) =>
          r.eval match {
            case Failure(reason) => Failure(reason)
            case Success(rvalue) => Success(lvalue + rvalue)
          }
      }

    case Subtraction(l, r) =>
      l.eval match {
        case Failure(reason) => Failure(reason)
        case Success(lvalue) =>
          r.eval match {
            case Failure(reason) => Failure(reason)
            case Success(rvalue) => Success(lvalue - rvalue)
          }
      }

    case Division(l, r) =>
      l.eval match {
        case Failure(reason) => Failure(reason)
        case Success(lvalue) =>
          r.eval match {
            case Failure(reason) => Failure(reason)
            case Success(0) => Failure("Divide zero")
            case Success(rvalue) => Success(lvalue / rvalue)
          }
      }

    case SquareRoot(e) =>
      e.eval match {
        case Failure(reason) => Failure(reason)
        case Success(value) if value < 0 => Failure("Negative root")
        case Success(value) => Success(Math.sqrt(value))
      }

    case Number(value) => Success(value)
  }
}

final case class Addition(left: Expression, right: Expression) extends Expression
final case class Subtraction(left: Expression, right: Expression) extends Expression
final case class Division(left: Expression, right: Expression) extends Expression
final case class SquareRoot(expression: Expression) extends Expression
final case class Number(value: Double) extends Expression

class CalculatorTests extends AnyFunSuite with Matchers {

  object calculator {
    def eval(expression: Expression): Result[Double] = expression.eval
  }

  test("Simple") {
    assert(Success(3.0) == calculator.eval(Addition(Number(1), Number(2))))
    assert(Success(8.0) == calculator.eval(Subtraction(Number(10), Number(2))))
    assert(Success(5.0) == calculator.eval(Division(Number(10), Number(2))))
    assert(Success(4.0) == calculator.eval(SquareRoot(Number(16))))

    assert(Failure[Double]("Divide zero") == calculator.eval(Division(Number(10), Number(0))))
    assert(Failure[Double]("Divide zero") == calculator.eval(Addition(Number(8), Division(Number(10), Number(0)))))

    assert(Failure("Negative root") == calculator.eval(SquareRoot(Number(-10.0))))


  }


}
