package chapter1

import java.util.Date

import cats.Eq
import cats.instances.int._
import cats.instances.string._
import cats.instances.long._
import cats.instances.option._
import cats.syntax.eq._
import cats.syntax.option._


object DateEq {
  implicit val dateEq: Eq[Date] =
    Eq.instance[Date] { (date1, date2) => 
      date1.getTime === date2.getTime
    }
}

object CatEq {
  implicit val catEq: Eq[Cat] = Eq.instance[Cat] {
    (cat1, cat2) => {
      cat1.name === cat2.name && cat1.age === cat2.age && cat1.color === cat2.color
    }
  }
}

object EqTest extends App {
  // val empty = List(1, 2, 3).map(Option(_)).filter(item => item == 1)
  val eqInt = Eq[Int]
  println(eqInt.eqv(123, 123))
  println(eqInt.eqv(123, 234))
  println(123 === 123)
  println(123 =!= 234)

  println((Some(1): Option[Int]) === (None: Option[Int]))
  println(Option(1) === Option.empty[Int])

  println(1.some === none[Int])
  println(1.some =!= none[Int])

  import DateEq._
  import java.time.Instant

  val x = Date.from(Instant.now())
  
  import java.util.concurrent.TimeUnit
  TimeUnit.SECONDS.sleep(1)
  val y = Date.from(Instant.now())

  println(x === x)
  println(x === y)

  import CatEq._
  val cat1 = Cat("milou", 15, "white")
  val cat2 = Cat("jeom", 3, "black and white and orange")

  val optionCat1 = Option(cat1)
  val optionCat2 = Option.empty[Cat]

  println(optionCat1 === optionCat2)
  println(cat1 === cat1)
}
