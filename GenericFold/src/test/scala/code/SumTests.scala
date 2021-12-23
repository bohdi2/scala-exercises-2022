package code

import org.scalatest.funsuite.AnyFunSuite

sealed trait Sum[+A, +B] {
  def fold[C](left: A => C, right: B => C): C = this match {
    case Left(a) => left(a)
    case Right(r) => right(r)
  }
}

final case class Left[A, B](value: A) extends Sum[A, B]

final case class Right[A, B](value: B) extends Sum[A, B]

class SumTests extends AnyFunSuite {

  test("Sum Fold") {

    def inBed(s: String): String = s"$s in bed"
    def inc(n: Int): Int = n + 1

    val left = Left("Error code 5")
    assert(left.fold(inBed, inc) == "Error code 5 in bed")

    val right = Right(5)
    assert(right.fold(inBed, inc) == 6)
  }

}
