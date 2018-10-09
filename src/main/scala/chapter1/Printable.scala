package chapter1

sealed trait Printable[A] {
  def format(value: A): String
}

object PrintableInstances {
  implicit val printableString: Printable[String] =
    new Printable[String] {
      def format(value: String): String = value
    }

  implicit val printableInt: Printable[Int] =
    new Printable[Int] {
      def format(value: Int): String = value.toString
    }
}

object Printable {
  def format[A](value: A)(implicit p: Printable[A]): String = 
    p.format(value)

  def print[A](value: A)(implicit p: Printable[A]): Unit =
    println(p.format(value))
}

object PrintableSyntax {
  implicit class PrintableOps[A](value: A) {
    def format(implicit p: Printable[A]): String =
      p.format(value)

    def print(implicit p: Printable[A]): Unit =
      println(p.format(value))
  }
}

final case class Cat(name: String, age: Int, color: String)

object Cat {
  import PrintableInstances._

  implicit val printableCat: Printable[Cat] =
    new Printable[Cat] {
      def format(value: Cat): String = {
        val name  = Printable.format(value.name)
        val age   = Printable.format(value.age)
        val color = Printable.format(value.color)
        s"$name is a $age year-old $color cat."
      }
    }
}

object Test extends App {
  import PrintableSyntax._
  val cat = Cat("milou", 15, "white")
  Printable.print(cat)
  cat.print

  import cats.Show
  import cats.instances.int._
  import cats.instances.string._

  val showInt: Show[Int] = Show.apply[Int]
  val showString: Show[String] = Show.apply[String]

  val intAsString: String = showInt.show(123)
  val stringAsString: String = showString.show("abc")

  import PrintableInstances._
  Printable.print(intAsString)
  Printable.print(stringAsString)

  import cats.syntax.show._

  val shownInt = 123.show
  val shownString = "abc".show
  Printable.print(shownInt)
  Printable.print(shownString)
}
