package sandbox.monoid

import cats.kernel.Monoid

case class Order(totalCost: Double, quantity: Double)

trait MonoidInstances {
  implicit val orderMonoidInstance = new Monoid[Order] {
    override def empty: Order = Order(0,0)
    override def combine(x: Order, y: Order): Order = Order(x.totalCost + y.totalCost, x.quantity + y.quantity)
  }
}

object MonoidInstances extends MonoidInstances