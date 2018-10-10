package chapter3

object FutureTest extends App {
  import scala.concurrent.{Future, Await}
  import scala.concurrent.ExecutionContext.Implicits.global
  import scala.concurrent.duration._

  val future: Future[String] =
    Future(123)
      .map(n => n + 1)
      .map(n => n * 2)
      .map (n => n + "!")

    println(Await.result(future, 1.second))
}

object FunctionTest extends App {
  import cats.instances.function._
  import cats.syntax.functor._

  val func1: Int => Double = (x: Int) => x.toDouble
  val func2: Double => Double = (x: Double) => x * 2

  println((func1 map func2)(1))
  println((func1 andThen func2)(1))
  println(func2(func1(1)))

  val func = ((x: Int) => x.toDouble)
    .map(x => x + 1)
    .map(x => x * 2)
    .map(x => x + "!")

  println(func(1))
}

object FutureFunctor {
  import cats.Functor
  import scala.concurrent.{Future, ExecutionContext}
  implicit def futureFunctor(implicit ex: ExecutionContext): Functor[Future] =
    new Functor[Future] {
      def map[A, B](value: Future[A])(func: A => B): Future[B] = value.map(func)
    }
}

object FunctorTest extends App {
  import cats.Functor
  import cats.instances.list._
  import cats.instances.option._

  val list1 = List(1, 2, 3)
  val list2 = Functor[List].map(list1)(_ * 2)
  val option1 = Option(123)
  val option2 = Functor[Option].map(option1)(_.toString)
  val func = (x: Int) => x + 1
  val liftedFunc = Functor[Option].lift(func)

  println(list2)
  println(option2)
  println(liftedFunc(Option(1)))

  import cats.syntax.functor._
  def doMath[F[_]](start: F[Int])(implicit functor: Functor[F]): F[Int] =
    start.map(n => n + 1 * 2)
  println(doMath(Option(20)))
  println(doMath(List(1, 2, 3)))
}
