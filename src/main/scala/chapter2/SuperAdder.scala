package chapter2


object SuperAdder {
  import cats.Monoid
  import cats.syntax.semigroup._

  def add[A: Monoid](items: List[A]): A =
    items.foldLeft(Monoid[A].empty)(_ |+| _)
}

case class Order(totalCost: Double, quantity: Double)
object Order {
  import cats.Monoid
  import cats.syntax.semigroup._
  import cats.syntax.show._
  import cats.instances.double._
  import cats.Show

  implicit val orderMonoid: Monoid[Order] = new Monoid[Order] {
    def combine(x: Order, y: Order): Order = {
      val totalCost = x.totalCost |+| y.totalCost
      val quantity = x.quantity |+| y.quantity
      Order(totalCost, quantity)
    }

    def empty: Order = Order(Monoid[Double].empty, Monoid[Double].empty)
  }

  implicit val orderShow = Show.show((order: Order) => {
      val totalCost = order.totalCost.show
      val quantity = order.quantity.show

      s"Total cost is ${totalCost}, Quantity is ${quantity}"
  })
}


object SuperAdderTest extends App {
  import cats.instances.option._
  import cats.instances.int._

  println(SuperAdder.add(List(1, 2, 3)))
  println(SuperAdder.add(List(Some(1), None, Some(2), None, Some(3))))

  import Order._
  import cats.syntax.show._
  println(SuperAdder.add(List(Order(1.0, 3.0), Order(2.5, 4.4))).show)
}
