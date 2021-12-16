package code

import org.scalatest._
import matchers.should._
import org.scalatest.funsuite.AnyFunSuite

class ExerciseTests extends AnyFunSuite with Matchers {

  test("infix operator") {
    assert("f" == "foo".take(1))
    assert("f" == ("foo" take 1))
  }

  test("square dance") {
    def square(n: Double): Double = n * n

    assert(25 == square(5))
  }

  test("if else Any Unit") {
    assert(25 == (if (true) 25 else 15))
    assert(25 == (if (true) 25))
    assert(() == (if (false) 25))

    var v: Any = if (true) 25
    assert(25 == v)
    v = if (false) 25
    assert(() == v)
  }

  test("if else types") {
    val v1 = if (true) 25 else "twenty four"
    assert(25 == v1)
    v1 shouldBe an [Int]

    val v2 = if (false) 25 else "twenty four"
    assert("twenty four" == v2)
    v2 shouldBe a[String]
  }

  test("counter 1") {
    class Counter(val n: Int) {
      def inc = new Counter(n+1)
      def dec = new Counter(n-1)
      def count: Int = n
    }

    assert(12 == new Counter(10).inc.dec.inc.inc.count)
  }

  test("counter 2") {
    class Counter(val n: Int) {
      def inc: Counter = inc()
      def dec: Counter = dec()

      def inc(delta: Int = 1) = new Counter(n+delta)
      def dec(delta: Int = 1) = new Counter(n-delta)
      def count: Int = n
    }

    assert(11 == new Counter(10).inc.dec(2).dec.inc.inc(2).count)
  }

  test("class Person") {
    object Person {
      def apply(firstName: String, lastName: String): Person = {
        new Person(firstName, lastName)
      }

      def apply(name: String): Person = {
        val parts = name.split(" +")
        new Person(parts(0), parts(1))
      }
    }

    class Person(val firstName: String, val lastName: String) {
      def name: String = firstName + " " + lastName
    }

    assert("Joe Blow" == new Person("Joe", "Blow").name)
    assert("Joe Blow" == Person("Joe", "Blow").name)
    assert("Joe Blow" == Person("Joe  Blow").name)
  }

  test("case class Person") {
    object Person {
      def apply(name: String): Person = {
        val parts = name.split(" +")
        new Person(parts(0), parts(1))
      }
    }

    case class Person(firstName: String, lastName: String) {
      def name: String = firstName + " " + lastName
    }

    assert("Joe Blow" == new Person("Joe", "Blow").name)
    assert("Joe Blow" == Person("Joe", "Blow").name)
    assert("Joe Blow" == Person("Joe  Blow").name)

    assert("Joe Brown" == Person("Joe", "Blow").copy(lastName = "Brown").name)
  }

  test("counter 3") {
    case class Counter(count: Int = 0) {
      def inc: Counter = inc()
      def dec: Counter = dec()

      def inc(delta: Int = 1): Counter = copy(count = count + delta)
      def dec(delta: Int = 1): Counter = copy(count = count - delta)
    }

    assert(11 == Counter(10).inc.dec(2).dec.inc.inc(2).count)
    assert(1 == Counter().inc.dec(2).dec.inc.inc(2).count)
  }

  test("ChipShop") {
    case class Cat(name: String, food: String)

    object ChipShop {
      def willServe(cat: Cat): Boolean = cat match {
        case Cat(_, "chips") => true
        case Cat(_, _) => false
      }
    }

    assert(ChipShop.willServe(Cat("Felix", "chips")))
    assert(!ChipShop.willServe(Cat("Garfield", "pizza")))
  }

}
