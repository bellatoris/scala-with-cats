package chapter3


trait Codec[A] {
  self =>

  def encode(value: A): String
  def decode(value: String): A
  def imap[B](dec: A => B, enc: B => A): Codec[B] =
    new Codec[B] {
      def encode(value: B): String = (self.encode _ compose enc)(value)
      def decode(value: String): B = (dec compose self.decode)(value)
    }
}

object Codec {
  def encode[A](value: A)(implicit c: Codec[A]): String = 
    c.encode(value)

  def decode[A](value: String)(implicit c: Codec[A]): A =
    c.decode(value)
}

object StringCodec {
  implicit val stringCodec: Codec[String] =
    new Codec[String] {
      def encode(value: String): String = value
      def decode(value: String): String = value
    }
}

object IntCodec {
  implicit val intCodec: Codec[Int] =
    StringCodec.stringCodec.imap(_.toInt, _.toString)
}

object BooleanCodec {
  implicit val booleanCodec: Codec[Boolean] =
    StringCodec.stringCodec.imap(_.toBoolean, _.toString)
}

object DoubleCodec {
  implicit val doubleCodec: Codec[Double] =
    StringCodec.stringCodec.imap(_.toDouble, _.toString)
}

object BoxCodec {
  implicit def boxCodec[A](implicit codec: Codec[A]): Codec[Box[A]] =
    codec.imap((a: A) => Box(a), (box: Box[A]) => box.value)
}

object CodecTest extends App {
  import DoubleCodec._
  import BoxCodec._
  import Codec._

  println(encode(123.4))
  println(decode[Double]("123.4"))
  println(encode(Box(123.4)))
  println(decode[Box[Double]]("123.4"))

  import cats.Contravariant
  import cats.Show
  import cats.instances.string._

  val showString = Show[String]
  val showSymbol = Contravariant[Show].contramap(showString)((sym: Symbol) => s"'${sym.name}")
  println(showSymbol.show('dave))

  import cats.syntax.contravariant._
  showString.contramap[Symbol](_.name).show('dave)

  import cats.Monoid
  import cats.syntax.invariant._
  import cats.syntax.semigroup._

  implicit val symbolMonoid: Monoid[Symbol] =
    Monoid[String].imap(Symbol.apply)(_.name)

  println(Monoid[Symbol].empty)
  println('a |+| 'few |+| 'words)

  type <=[B, A] = A => B
  type F[A] = Double <= A

  val func1 = (x: Int) => x.toDouble
  val func2 = (y: Double) => y * 2
  val func2b: Double <= Double = func2
  
  import cats.syntax.contravariant._
  import cats.instances.function._
  val func3c: Double <= Int = func2b.contramap(func1)
}
