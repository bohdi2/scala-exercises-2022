package core

import org.scalatest._
import matchers.should._
import org.scalatest.funsuite.AnyFunSuite

sealed trait Result[T]
final case class Success[T](value: T) extends Result[T]
final case class Failure[T](reason: String) extends Result[T]

sealed trait LinkedList {

  def contains(n: Int): Boolean = this match {
    case End() => false
    case Pair(head, _) if head == n => true
    case Pair(_, tail) => tail.contains(n)
  }

  def apply(n: Int): Result[Int] = this match {
    case End() => Failure("Index out of bounds")
    case Pair(head, _) if n == 0 => Success(head)
    case Pair(_, tail) => tail.apply(n - 1)
  }

  def fold[T](end: T, f: (Int, T) => T): T = this match {
    case End() => end
    case Pair(head, tail) => f(head, tail.fold(end, f))
  }

  def length: Int = fold[Int](0, (_, tail) => tail + 1)

  def sum: Int = fold[Int](0, _ + _)

  def product: Int = fold[Int](1, _ * _)

  def double: LinkedList = fold[LinkedList](End(), (head: Int, accum: LinkedList) => Pair(2 * head, accum))


}

final case class End() extends LinkedList

final case class Pair(head: Int, tail: LinkedList) extends LinkedList

class LinkedListTests extends AnyFunSuite with Matchers {

  test("one") {
    val example = Pair(1, Pair(2, Pair(3, End())))

    assert(example.length == 3)
    assert(Pair(2, Pair(4, Pair(6, End()))) == example.double)
    assert(6 == example.product)
    assert(6 == example.sum)

    assert(example.contains(3))
    assert(!example.contains(4))
    assert(!End().contains(0))

    assert(Success(1) == example(0))
    assert(Success(2) == example(1))
    assert(Failure[Int]("Index out of bounds") == example(7))
  }

}

