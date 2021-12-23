package code

import org.scalatest.funsuite.AnyFunSuite

class RunLengthTests extends AnyFunSuite {

  def x(s: String): List[(String, Int)] = {
    s.map(c => (c.toString, 1))
      .foldRight(List[(String, Int)]()) {
        (t, accum) => (t, accum) match {
          case ((letter, count), Nil) => (letter, 1) :: Nil
          case ((letter, count), (hd :: tl)) if (hd._1 == letter) => (letter, count+1) :: tl
          case ((letter, count), accum) => (letter, 1) :: accum
          }
      }
  }

  def func[X](xs: Seq[X]): List[(Int, X)] = xs match {
    case Nil => Nil
    case y :: ys => func(ys) match {
      case (c, `y`) :: rest => (c + 1, y) :: rest
      case rest => (1, y) :: rest
    }
  }

  case class Accum(count: Int, prefix: Char, builder: StringBuilder)

  def encode(s: String): String = {
    (1 until s.length).foldLeft(Accum(1, s(0), new StringBuilder)) {
      case (Accum(len, c, sb), index) if c != s(index) => {
        sb.append(len)
        sb.append(c);
        Accum(1, s(index), sb)
      }
      case (Accum(len, c, sb), _) => Accum(len + 1, c, sb)
    } match {
      case Accum(len, c, sb) => sb.append(len); sb.append(c); sb.toString
    }
  }

  case class Accum2(count: Int, prefix: Char, result: String) {
    def encode: String = s"${result}${count}${prefix}"
  }

  def encode2(s:String): String = {
    s.foldLeft(Accum2(0, s(0), "")) {
      (accum, c) => accum match {
        //case Accum2(i, p, ss) => if (p == c) Accum2(i + 1, p, s) else Accum2(1, c, s"${ss}${i}${p}")
        case Accum2(i, p, ss) => if (p == c) Accum2(i + 1, p, s) else Accum2(1, c, accum.encode)
      }
    }

    match {case Accum2(i, p, s) => s"${s}${i}${p}"}
  }


  test("run length") {

    var expected = List(("x", 1), ("y", 1), ("z", 2), ("y", 1))

    println(x("122333"))
    println(x("xyzzy"))



    println(func("12233".map(_.toString).toList))

    println(encode("xyzzy"))
    println(encode2("xyzzy"))


  }
}