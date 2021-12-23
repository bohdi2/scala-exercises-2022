package code

import org.scalatest.funsuite.AnyFunSuite

object ListMonadTests {

  sealed trait LinkedList[+A] {

    def fold[B](end: B)(f: (A, B) => B): B = this match {
      case Pair(head, tail) => f(head, tail.fold(end)(f))
      case End => end
    }

    def map[B](fn: A => B): LinkedList[B] =
      this match {
        case Pair(hd, tl) => Pair(fn(hd), tl.map(fn))
        case End => End
      }


  }


  final case class Pair[A](hd: A, tl: LinkedList[A]) extends LinkedList[A]
  final case object End extends LinkedList[Nothing]
}


class ListMonadTests extends AnyFunSuite {
  import code.ListMonadTests._

  test("List Map") {

    val original = Pair(1, Pair(2, Pair(3, End)))
    val expected = Pair(2, Pair(4, Pair(6, End)))

    assert(original.map(_*2) == expected)

    // Need flatMap
    println(original.map(value => Pair(value, End)))
  }

  test("option") {
    import scala.util.Try

    val opt1 = Option(1)
    val opt2 = Option(2)
    val opt3 = Option(3)

    val v = for {
      v1 <- opt1
      v2 <- opt2
      v3 <- opt3
    } yield v1 + v2 + v3

    assert(v == Option(6))
  }

  test("Seq") {
    val seq1 = Seq(1)
    val seq2 = Seq(2)
    val seq3 = Seq(3)

    val v = for {
      v1 <- seq1
      v2 <- seq2
      v3 <- seq3
    } yield v1 + v2 + v3

    assert(v == List(6))
  }

  test("try") {
    import scala.util.Try

    val try1 = Try(1)
    val try2 = Try(2)
    val try3 = Try(3)

    val v = for {
      v1 <- try1
      v2 <- try2
      v3 <- try3
    } yield v1 + v2 + v3

    assert(v == Try(6))
  }

}
