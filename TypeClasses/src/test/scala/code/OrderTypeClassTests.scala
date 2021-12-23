package code

import code.OrderTypeClassTests.Order
import org.scalatest.funsuite.AnyFunSuite

object OrderTypeClassTests {
  final case class Order(units: Int, unitPrice: Double) {
    def totalPrice: Double = units * unitPrice
  }

  object Order {
    implicit val lessThanOrdering: Ordering[Order] = Ordering.fromLessThan[Order]((a, b) => a.totalPrice < b.totalPrice)
  }
}

object OrderUnitPriceOrdering {
  implicit val unitPriceOrdering: Ordering[Order] = Ordering.fromLessThan[Order]((a, b) => a.unitPrice < b.unitPrice)
}

object OrderUnitOrdering {
  implicit val unitPriceOrdering: Ordering[Order] = Ordering.fromLessThan[Order]((a, b) => a.units < b.units)
}



class OrderTypeClassTests extends AnyFunSuite {

  val order1: Order = Order(5, 10.0)
  val order2: Order = Order(2, 12.0)
  val order3: Order = Order(7, -2.0)

  val orders = List(order1, order2, order3)

  test("total price") {
    assert(orders.sorted == List(order3, order2, order1))
  }

  test("unit price") {
    import code.OrderUnitPriceOrdering._

    assert(orders.sorted == List(order3, order1, order2))
  }

  test("unit") {
    import code.OrderUnitOrdering._

    assert(orders.sorted == List(order2, order1, order3))
  }

}


