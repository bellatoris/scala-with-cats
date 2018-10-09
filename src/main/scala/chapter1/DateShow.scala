package chapter1

import java.util.Date
import java.time.Instant
import cats.Show
import cats.syntax.show._

object DateShow {
  implicit val dateShow: Show[Date] = 
    new Show[Date] {
      def show(date: Date): String =
        s"${date.getTime}ms since the epoch."
    }
}

object DateShow2 {
  implicit val dateShow: Show[Date] = 
    Show.show(date => s"${date.getTime}ms since the spoch.")
}

object CatShow {
  import cats.instances.string._
  import cats.instances.int._

  implicit val catShow: Show[Cat] = {
    Show.show((cat: Cat) => {
      val name = cat.name.show
      val age = cat.age.show
      val color = cat.color.show

      s"${name} is a ${age} year-old ${color} cat."
    })
  }
}

object ShowTest extends App {
  val now = Instant.now()
  val date = Date.from(now)

  {
    import DateShow._
    println(date.show)
  }
  
  {
    import DateShow2._
    println(date.show)
  }

  import CatShow._
  val cat = Cat("milou", 15, "white")
  println(cat.show)
}
