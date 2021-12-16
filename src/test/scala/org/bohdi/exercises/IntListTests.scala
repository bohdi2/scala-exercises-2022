package org.bohdi.exercises

import org.scalatest._
import matchers.should._
import org.scalatest.funsuite.AnyFunSuite

sealed trait IntList {

  def length: Int = this match {
    case End => 0
    case Pair(head, tail) => 1 + tail.length
  }


  def double: IntList = this match {
    case End => End
    case Pair(head, tail) => new Pair(2 * head, tail.double)
  }

  def product: Int = this match {
    case End => 1
    case Pair(head, tail) => head * tail.product
  }

  def sum: Int = this match {
    case End => 0
    case Pair(head, tail) => head + tail.sum
  }
}

final case object End extends IntList
final case class Pair(head: Int, tail: IntList) extends IntList

class IntListTests extends AnyFunSuite with Matchers {

  test("one") {
    val example = Pair(1, Pair(2, Pair(3, End)))

    assert(3 == example.length)
    assert(Pair(2, Pair(4, Pair(6, End))) == example.double)
    assert(6 == example.product)
    assert(6 == example.sum)
  }

}
