package code

import org.scalatest.funsuite.AnyFunSuite

sealed trait Maybe[+A] {
  def fold[B](justF: A => B, emptyF: B): B = this match {
    case Just(value) => justF(value)
    case None => emptyF
  }
}

final case class Just[A](value: A) extends Maybe[A]
case object None extends Maybe[Nothing]

class GenericMaybeTests extends AnyFunSuite {

  test("Maybe") {

    assert(Just("Hello").value == "Hello")

  }

  test("Maybe Fold") {
    val something = Just("Apple")
    val nothing = None

    assert(something.fold((value: String) => s"${value}s", "foo") == "Apples")
    assert(nothing.fold((value: String) => s"${value}s", "foo") == "foo")
  }

}
