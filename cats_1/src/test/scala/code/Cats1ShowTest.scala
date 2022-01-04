package code

import cats._
import cats.instances.int._
import cats.instances.string._
import cats.syntax.show._
import org.scalatest.funsuite.AnyFunSuite

object Cats1ShowTest {
  final case class Cat(name: String, age: Int, color: String)

  implicit val catShow: Show[Cat] = Show[Cat] { cat =>
    val name = cat.name.show
    val age = cat.age.show
    val color = cat.color.show

    s"${name} is a ${age} year-old ${color} cat."
  }
}

class Cats1ShowTest extends AnyFunSuite {
  import code.Cats1ShowTest._

  test("cats1 show basic") {
    val cat = Cat("Beano", 22, "sable")
    assert(cat.show == "Beano is a 22 year-old sable cat.")
  }

  test("cats1 show syntax") {
    val cat = Cat("Beano", 22, "sable")
    assert(cat.show == "Beano is a 22 year-old sable cat.")
  }

}
