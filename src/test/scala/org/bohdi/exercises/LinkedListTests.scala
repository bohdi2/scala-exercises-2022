package org.bohdi.exercises


import org.scalatest._
import matchers.should._
import org.scalatest.funsuite.AnyFunSuite

sealed trait LinkedList[T] {

  def length: Int = this match {
    case EndOfList() => 0
    case ListPair(_, tail) => 1 + tail.length
  }

  def contains(n: T): Boolean = this match {
    case EndOfList() => false
    case ListPair(head, _) if head == n => true
    case ListPair(_, tail) => tail.contains(n)
  }

  def apply(n: Int): Result[T] = this match {
    case EndOfList() => Failure("Index out of bounds")
    case ListPair(head, _) if n == 0 => Success(head)
    case ListPair(_, tail) => tail.apply(n-1)

  }

//  def double: LinkedList[T] = this match {
//    case EndOfList => EndOfList
//    case ListPair(head, tail) => new ListPair(2 * head, tail.double)
//  }
//
//  def product: T = this match {
//    case EndOfList => 1
//    case ListPair(head, tail) => head * tail.product
//  }
//
//  def sum: T = this match {
//    case EndOfList => 0
//    case ListPair(head, tail) => head + tail.sum
//  }
}

final case class EndOfList[T]() extends LinkedList[T]
final case class ListPair[T](head: T, tail: LinkedList[T]) extends LinkedList[T]

class LinkedListTests extends AnyFunSuite with Matchers {

  test("one") {
    val example = ListPair[Int](1, ListPair(2, ListPair(3, EndOfList[Int]())))

    assert(3 == example.length)
//    assert(ListPair(2, ListPair(4, ListPair(6, EndOfList))) == example.double)
//    assert(6 == example.product)
//    assert(6 == example.sum)

    assert(example.contains(3))
    assert(!example.contains(4))
    assert(!EndOfList().contains(0))

    assert(Success(1) == example(0))
    assert(Success(2) == example(1))
    assert(Failure[Int]("Index out of bounds") == example(7))
  }

}

