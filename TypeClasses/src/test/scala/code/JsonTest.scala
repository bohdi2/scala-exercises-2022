package code

import org.scalatest.funsuite.AnyFunSuite

import java.util.Date

object JsonTest {

  sealed trait Visitor {
    def id: String
    def createdAt: Date
    def age: Long = new Date().getTime - createdAt.getTime
  }

  final case class Anonymous(id: String, createdAt: Date = new Date()) extends Visitor

  final case class User(id: String, email: String, createdAt: Date = new Date()) extends Visitor

  // ------------------

  sealed trait JsValue {
    def stringify: String
  }

  final case class JsObject(values: Map[String, JsValue]) extends JsValue {
    override def stringify: String = values.map {
      case (name, value) => s"\"$name\":${value.stringify}"
    }
      .mkString("{", ",", "}")
  }

  final case class JsString(value: String) extends JsValue {
    override def stringify: String = s"\"${value.replaceAll("\\|\"", "\\\\$1")}\""
  }

  sealed trait JsWriter[A] {
    def write(value: A): JsValue
  }

  implicit object StringWriter extends JsWriter[String] {
    override def write(value: String): JsValue = JsString(value)
  }

  implicit object DateWriter extends JsWriter[Date] {
    override def write(value: Date): JsValue = JsString(value.toString)
  }

  implicit object AnonymousJsWriter extends JsWriter[Anonymous] {
    def write(user: Anonymous): JsValue = {
      JsObject(Map(
        "id" -> user.id.toJson,
        "createdAt" -> user.createdAt.toJson
      ))
    }
  }

  implicit object UserJsWriter extends JsWriter[User] {
    def write(user: User): JsValue = {
      JsObject(Map(
        "id" -> user.id.toJson,
        "email" -> user.email.toJson,
        "createdAt" -> user.createdAt.toJson)
      )
    }
  }

  implicit object VisitorWriter extends JsWriter[Visitor] {
    def write(value: Visitor) = value match {
      case anon: Anonymous => anon.toJson
      case user: User      => user.toJson
    }
  }

  implicit class JsUtil[A](value: A) {
    def toJson(implicit jsWriter: JsWriter[A]): JsValue = jsWriter.write(value)
  }

}

class JsonTest extends AnyFunSuite {
  import code.JsonTest._

  test("basic") {
    val obj = JsObject(Map("foo" -> JsString("a"), "bar" -> JsString("b"), "baz" -> JsString("c")))
    println(obj.stringify)
  }

  test("x") {
    val visitors: Seq[Visitor] = Seq(Anonymous("001", new Date), User("003", "dave@xample.com", new Date))

    visitors.map(v => v.toJson).foreach(println(_))

  }

}
