package code

import org.scalatest.funsuite.AnyFunSuite

import scala.language.implicitConversions

object IntEnrichmentTest {

  class IntOps(n: Int) {
    def yeah(): Unit = times(_ => println("Oh Yeah"))

    def times(f: Int => Unit): Unit = for {ii <- 0 until n} f(ii)
  }

  implicit def intToIntOps(value: Int): IntOps = new IntOps(value)

}


class IntEnrichmentTest extends AnyFunSuite {
  import code.IntEnrichmentTest._

  test("yeah") {
    3.yeah()
  }

  test("times") {
    3.times(i => println(s"Look - it's the number $i!"))
  }

}
