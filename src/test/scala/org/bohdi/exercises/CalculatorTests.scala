package org.bohdi.exercises

import org.scalatest._
import matchers.should._
import org.scalatest.funsuite.AnyFunSuite

sealed trait Result
final case class Success(value: Double) extends Result
final case class Failure(reason: String) extends Result

sealed trait Expression {
  def eval: Double = this match {
    case Addition(l, r) => l.eval + r.eval
    case Subtraction(l, r) => l.eval - r.eval
    case Division(l, r) =>l.eval / r.eval
    case SquareRoot(e) => Math.sqrt(e.eval)
    case Number(e) => e
    //case Error(e) => e
  }
}
final case class Addition(left: Expression, right: Expression) extends Expression
final case class Subtraction(left: Expression, right: Expression) extends Expression
final case class Division(left: Expression, right: Expression) extends Expression
final case class SquareRoot(expression: Expression) extends Expression
final case class Number(value: Double) extends Expression
//final case class Error(value: String) extends Expression

//case object Number {
//  def apply(value: Double): Number = Number(Success(value))
//}

class CalculatorTests extends AnyFunSuite with Matchers {

  object calculator {
    def eval(expression: Expression): Result = {
      expression.eval match {
        case value: Double => Success(value)
        //case Error(reason) => Failure(reason)
        //case _ => Failure("Unresolved expression")
      }
    }
  }

  test("Simple") {
    assert(Success(3.0) == calculator.eval(Addition(Number(1), Number(2))))
    assert(Success(8.0) == calculator.eval(Subtraction(Number(10), Number(2))))
    assert(Success(5.0) == calculator.eval(Division(Number(10), Number(2))))
    assert(Success(4.0) == calculator.eval(SquareRoot(Number(16))))

    //assert(Failure("Divide zero") == calculator.eval(Division(Number(10), Number(0))))
    //assert(Failure("Divide zero") == calculator.eval(Addition(Number(8), Division(Number(10), Number(0)))))

  }


}
