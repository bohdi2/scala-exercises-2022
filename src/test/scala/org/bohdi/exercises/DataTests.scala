package org.bohdi.exercises

import org.scalatest._
import matchers.should._
import org.scalatest.funsuite.AnyFunSuite

import scala.annotation.tailrec

class DataTests extends AnyFunSuite with Matchers {

  test("Shapes") {
    import math.Pi

    trait Shape {
      def sides: Int
      def perimeter: Double
      def area: Double
    }

    case class Circle(radius: Double) extends Shape {
      override def sides = 1
      override def perimeter: Double = 2*radius*Pi
      override def area: Double = Pi*radius*radius
    }

    sealed trait Rectangular extends Shape {
      def width: Double
      def height: Double
      override def sides = 4
      override def perimeter: Double = 2 * (width + height)
      override def area: Double = width * height
    }

    case class Square(size: Double) extends Rectangular {
      val width: Double = size
      val height: Double = size
    }

    case class Rectangle(width: Double, height: Double) extends Rectangular

    assert(1 == Circle(1).sides)
    assert(2*Pi == Circle(1).perimeter)
    assert(Pi == Circle(1).area)

    assert(4 == Square(1).sides)
    assert(4 == Square(1).perimeter)
    assert(1 == Square(1).area)

    assert(4 == Rectangle(1, 2).sides)
    assert( 6 == Rectangle(1, 2).perimeter)
    assert(2 == Rectangle(1, 2).area)
  }

  test("Sealed Shapes") {
    import math.Pi

    sealed trait Shape {
      def sides: Int
      def perimeter: Double
      def area: Double
    }

    final case class Circle(radius: Double) extends Shape {
      override def sides = 1
      override def perimeter: Double = 2*radius*Pi
      override def area: Double = Pi*radius*radius
    }

    sealed trait Rectangular extends Shape {
      def width: Double
      def height: Double
      override def sides = 4
      override def perimeter: Double = 2 * (width + height)
      override def area: Double = width * height
    }

    final case class Square(size: Double) extends Rectangular {
      val width: Double = size
      val height: Double = size
    }

    final case class Rectangle(width: Double, height: Double) extends Rectangular

    object Draw {
      def apply(shape: Shape): String = shape match {
        case Circle(radius) => s"A circle of radius ${radius}"
        case Square(side) => s"A Square with sides of ${side}"
        case Rectangle(width, height) => s"A Rectangle with width ${width} and height of ${height}"
      }
    }

    assert("A circle of radius 1.0" == Draw(Circle(1)))
    assert("A Square with sides of 1.0" == Draw(Square(1)))
    assert("A Rectangle with width 1.0 and height of 2.0" == Draw(Rectangle(1, 2)))
  }

  test("Divide Result") {
    sealed trait DivisionResult
    final case class Finite(result: Double) extends DivisionResult
    final case object Infinite extends DivisionResult

    object divide {
      def apply(a: Int, b: Int): DivisionResult = if (b == 0) Infinite else new Finite(a/b)
    }

    assert(Finite(2/3) == divide(2, 3))
    assert(Infinite == divide(2, 0))
  }

  test("Calculation") {
    sealed trait Result
    final case class Success(result: Int) extends Result
    final case class Failure(reason: String) extends Result

    object Calculation {
      def +(a: Result, b: Int): Result = a match {
        case Success(aa) => Success(aa + b)
        case Failure(reason) => Failure(reason)
      }

      def -(a: Result, b: Int): Result = a match {
        case Success(aa) => Success(aa - b)
        case Failure(reason) => Failure(reason)
      }

      def /(a: Result, b: Int): Result = a match {
        case Success(aa) => if (b != 0) Success(aa/b) else Failure("Divide by zero")
        case Failure(reason) => Failure(reason)
      }
    }

    assert(Success(5) == Calculation.+(Success(3), 2))
    assert(Success(5) == Calculation.-(Success(8), 3))
    assert(Success(2) == Calculation./(Success(8), 3))
    assert(Failure("Divide by zero") == Calculation./(Success(8), 0))
  }

  test("Recursion") {
    sealed trait IntList
    case object End extends IntList
    final case class Pair(head: Int, tail: IntList) extends IntList

    @tailrec
    def sum(list: IntList, accum: Int = 0): Int = list match {
      case End => accum
      case Pair(head, tail) => sum(tail, head + accum)
    }

    val example = Pair(1, Pair(2, Pair(3, End)))
    assert(6 == sum(example))
    assert(5 == sum(example.tail))
    assert(0 == sum(End))

  }

}
