package code

import org.scalatest.funsuite.AnyFunSuite

class SubGroupTests extends AnyFunSuite {

  def prefixes(s: String): Seq[String] = (1 to s.length).map(s.slice(0, _))

  def suffixes(s: String): Seq[String] = (0 until s.length).map(s.slice(_, s.length))

  def groups(s: String): Seq[String] = {
    prefixes(s).flatMap(suffixes)
  }




  test("Hello") {

    println(s"prefixes applied to abcd ${prefixes("abcd")}")
    println(s"suffixes applied to abcd ${suffixes("abcd")}")
    println(s"x flatmap ${prefixes("abcd").flatMap(prefixes)}")
    println(s"Groups for abcd: ${groups("abcd")}")

  }
}