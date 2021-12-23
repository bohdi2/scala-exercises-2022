package code

import org.scalatest.funsuite.AnyFunSuite
import scala.math.Ordering
import cats.Show

class TypeClassesTests extends AnyFunSuite {

  test("Type classes") {
    val absOrdering = Ordering.fromLessThan[Int](Math.abs(_) < Math.abs(_))

    assert(List(-4, -1, 0, 2, 3).sorted(absOrdering) == List(0, -1, 2, 3, -4))
    assert(List(-4, -3, -2, -1).sorted(absOrdering) == List(-1, -2, -3, -4))
  }

  test("Type classes: implicit") {
    implicit val absOrdering: Ordering[Int] = Ordering.fromLessThan[Int](Math.abs(_) < Math.abs(_))

    assert(List(-4, -1, 0, 2, 3).sorted == List(0, -1, 2, 3, -4))
    assert(List(-4, -3, -2, -1).sorted == List(-1, -2, -3, -4))
  }

  test("Rational") {

    final case class Rational(numerator: Int, denominator: Int) {
      def asDouble: Double = numerator.toDouble / denominator.toDouble
    }

    implicit val rOrdering: Ordering[Rational] = Ordering.fromLessThan[Rational]((r1, r2) => r1.asDouble < r2.asDouble)

    val original = List(Rational(1, 2), Rational(3, 4), Rational(1, 3))
    val expected = List(Rational(1, 3), Rational(1, 2), Rational(3, 4))

    assert(original.sorted == expected)
  }

}