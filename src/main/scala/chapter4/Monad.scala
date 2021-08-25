package chapter4


object OptionMonad extends App {
  def parseInt(str: String): Option[Int] =
    scala.util.Try(str.toInt).toOption

  def divide(a: Int, b: Int): Option[Int] =
    if (b == 0) None else Some(a / b)

  def stringDivideBy(aStr: String, bStr: String): Option[Int] = {
    parseInt(aStr).flatMap { aNum =>
      parseInt(bStr).flatMap { bNum =>
        divide(aNum, bNum)
      }
    }
  }

  def stringDivideBy2(aStr: String, bStr: String): Option[Int] = for {
    aNum <- parseInt(aStr)
    bNum <- parseInt(bStr)
    ans  <- divide(aNum, bNum)
  } yield ans

  println(stringDivideBy("10", "2"))
  println(stringDivideBy("10", "0"))
  println(stringDivideBy("10", "foo"))
  println(stringDivideBy("bar", "2"))
}

object ListMonad extends App {
  val result = for {
    x <- (1 to 3).toList
    y <- (4 to 5).toList
  } yield (x, y)

  println(result)
}

object FutureMonad extends App {
  import scala.concurrent.Future
  import scala.concurrent.ExecutionContext.Implicits.global
  // import scala.concurrent.duration._

  def doSomethingLongRunning: Future[Int] = Future { 1000 }
  def doSomethingElseLongRunning: Future[Int] = Future { 4 }

  def doSomethingVeryLongRunning: Future[Int] =
    for {
      result1 <- doSomethingLongRunning
      result2 <- doSomethingElseLongRunning
    } yield result1 + result2
}
