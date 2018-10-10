package chapter3


trait Printable[A] {
  self =>

  def format(value: A): String
  def contramap[B](func: B => A): Printable[B] =
    new Printable[B] {
      def format(value: B): String = self.format(func(value))
    }
}

object Printable {
    def format[A](value: A)(implicit p: Printable[A]): String =
      p.format(value)
}

object StringPrintable {
  implicit val stringPrintable: Printable[String] =
    new Printable[String] {
      def format(value: String): String =
        "\"" + value + "\""
    }
}

object BooleanPrintable {
  implicit val booleanPrintable: Printable[Boolean] =
    new Printable[Boolean] {
      def format(value: Boolean): String = 
        if (value) "yes" else "no"
    }
}

final case class Box[A](value: A)

object BoxPrintable {
  implicit def boxPrintable[A](implicit p: Printable[A]): Printable[Box[A]] =
    p.contramap[Box[A]]((box: Box[A]) => box.value)
}

object PrintableTest extends App {
  import StringPrintable._
  import BooleanPrintable._
  import BoxPrintable._

  println(Printable.format("hello"))
  println(Printable.format(true))
  println(Printable.format(Box("hello")))
}
