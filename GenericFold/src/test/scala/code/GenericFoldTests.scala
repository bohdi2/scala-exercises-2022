package code

import org.scalatest.funsuite.AnyFunSuite

sealed trait LinkedList[+A] {
  def fold[B](end: B, f: (A, B) => B): B = this match {
    case End => end
    case Pair(head, tail) => f(head, tail.fold(end, f))
  }
}
final case class Pair[+A](head: A, tail: LinkedList[A]) extends LinkedList[A]
case object End extends  LinkedList[Nothing]

sealed trait Tree[+A] {
  def fold[B](nodeF: (B, B, A) => B, leafF: A => B): B
}

final case class Node[A](leftTree: Tree[A], rightTree: Tree[A], value: A) extends Tree[A] {
  def fold[B](nodeF: (B, B, A) => B, leafF: A => B): B = {
    nodeF(
      leftTree.fold(nodeF, leafF),
      rightTree.fold(nodeF, leafF),
      value
    )
  }
}

final case class Leaf[A](leafValue: A) extends Tree[A] {
  def fold[B](nodef: (B, B, A) => B, leafF: A => B): B = {
    leafF(leafValue)
  }
}


class GenericFoldTests extends AnyFunSuite {
  test("tree") {
    val tree: Tree[String] =
      Node(
        Node(
          Leaf("To"),
          Leaf("iterate"),
          " "
        ),
        Node(
          Node(
            Leaf("is"),
            Leaf("human,"),
            " "
          ),
          Node(
            Leaf("to"),
            Node(
              Leaf("recurse"),
              Leaf("divine"),
              " "
            ),
            " "
          ),
          " "
        ),
        " "
      )

    val expected = "To iterate is human, to recurse divine"
    assert(tree.fold((a: String, b: String, c: String) => a + c + b, str => str) == expected)
  }
}