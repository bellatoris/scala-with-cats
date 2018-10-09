package chapter2


object CatsMonoidTest extends App {
  import cats.Monoid
  import cats.instances.string._

  println(Monoid[String].combine("Hi ", "there"))
  println(Monoid[String].empty)

  println(Monoid.apply[String].combine("Hi ", "there"))
  println(Monoid.apply[String].empty)

  import cats.Semigroup
  println(Semigroup[String].combine("Hi ", "there"))

  import cats.instances.int._
  import cats.instances.option._

  println(Monoid[Int].combine(32, 10))
  val a = Option(22)
  val b = Option(20)
  println(Monoid[Option[Int]].combine(a, b))

  import cats.syntax.semigroup._

  val stringResult = "Hi " |+| "there" |+| Monoid[String].empty
  val intResult = 1 |+| 2 |+| Monoid[Int].empty

  println(stringResult)
  println(intResult)
}

