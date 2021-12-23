package code

import code.Cats1TypeClassesTest.PrintableSyntax.PrintableOps
import org.scalatest.funsuite.AnyFunSuite

object Cats1TypeClassesTest {

  trait Printable[A] {
    def format(a: A): String
  }

  object PrintableInstances {
    implicit val intPrintable: Printable[Int] = new Printable[Int] {
      def format(n: Int): String = n.toString
    }

    implicit val stringPrintable: Printable[String] = new Printable[String] {
      def format(s: String): String = s
    }

    implicit val catPrintable: Printable[Cat] = new Printable[Cat] {
      def format(cat: Cat):String = {
        val name = Printable.format(cat.name)
        val age = Printable.format(cat.age)
        val color = Printable.format(cat.color)

        s"${name} is a ${age} year-old ${color} cat."
      }
    }
  }

  object Printable {
    def format[A](a: A)(implicit printable: Printable[A]): String = printable.format(a)

    def print[A](a: A)(implicit printable: Printable[A]): Unit = println(printable.format(a))
  }

  object PrintableSyntax {
    implicit class PrintableOps[A](value: A) {
      def format(implicit p: Printable[A]): String = p.format(value)
      def print(implicit p: Printable[A]): Unit = println(p.format(value))
    }
  }

  final case class Cat(name: String, age: Int, color: String)
}


class Cats1TypeClassesTest extends AnyFunSuite {
  import code.Cats1TypeClassesTest._
  import Cats1TypeClassesTest.PrintableInstances._

  test("cats1 basic") {
    val cat = Cat("Beano", 22, "sable")
    assert(Printable.format(cat) == "Beano is a 22 year-old sable cat.")
  }

  test("cats1 syntax") {
    val cat = Cat("Beano", 22, "sable")
    assert(cat.format == "Beano is a 22 year-old sable cat.")
  }

}
