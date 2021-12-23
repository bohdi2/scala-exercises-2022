package code

import org.scalatest.funsuite.AnyFunSuite

object EqualsTests {

  trait Equals[A] {
    def equal(left: A, right: A): Boolean
  }

  case class Person(name: String, email: String)

  object Person {
    implicit object EmailEquals extends Equals[Person] {
      def equal(left: Person, right: Person): Boolean = left.email == right.email
    }
  }

  object NameAndEmailImplicit {
    implicit object NameEmailEquals extends Equals[Person] {
      def equal(left: Person, right: Person): Boolean = left == right
    }
  }

  object Eq {
    def apply[A](left: A, right: A)(implicit e: Equals[A]): Boolean = e.equals(left, right)
  }

}

class EqualsTests extends AnyFunSuite {
  import code.EqualsTests._

  val janeDoe: Person = Person("Jane", "JaneDoe@gmail.com")
  val johnBrown: Person = Person("John", "JohnBrown@gmail.com")
  val johnDoe: Person = Person("John", "JohnDoe@gmail.com")

  test("Email Equals") {
    assert(Person.EmailEquals.equal(janeDoe, janeDoe))
    assert(!Person.EmailEquals.equal(janeDoe, johnDoe))
  }

  test("Name Email Equals") {
    assert(NameAndEmailImplicit.NameEmailEquals.equal(janeDoe, janeDoe))
    assert(!NameAndEmailImplicit.NameEmailEquals.equal(janeDoe, johnDoe))
  }

  test("Eq") {
    Eq(Person("Noel", "noel@example.com"), Person("Noel", "noel@example.com"))(NameAndEmailImplicit.NameEmailEquals)
    Eq(Person("Noel", "noel@example.com"), Person("Noel", "noel@example.com"))
  }



}
