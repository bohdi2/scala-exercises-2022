package code

import cats.Eq
import cats.instances.int._ // for Eq
//import cats.instances.option._ // for Eq
import cats.instances.string._ // for Eq
import cats.syntax.eq._ // For ===
import org.scalatest.funsuite.AnyFunSuite

object Cats1EqTest {
  final case class Cat(name: String, age: Int, color: String)

  implicit val catEqual: Eq[Cat] = Eq.instance[Cat] {
    (cat1, cat2) =>
      (cat1.name === cat2.name) &&
      (cat1.age === cat2.age) &&
      (cat1.color === cat2.color)
    }
}

class Cats1EqTest extends AnyFunSuite {
  import code.Cats1EqTest._

  test("f") {
    val cat1 = Cat("Garfield",   38, "orange and black")
    val cat2 = Cat("Heathcliff", 33, "orange and black")
    //val cat3 = Cat("Heathcliff", 33, "orange and black")

    cat1 =!= cat2
    cat1 === cat2

    val optionCat1 = Option(cat1)
    val optionCat2 = Option.empty[Cat]

    assert(optionCat1 =!= optionCat2)

  }

}
