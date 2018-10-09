package chapter2


trait Monoid[A] extends Semigroup[A] {
  def empty: A
}

object Monoid {
  def associativeLaw[A](x: A, y: A, z: A)(implicit m: Monoid[A]): Boolean = {
    m.combine(x, m.combine(y, z)) == m.combine(m.combine(x, y), z)
  }
  
  def identityLaw[A](x: A)(implicit m: Monoid[A]): Boolean = {
    (m.combine(x, m.empty) == x) && (m.combine(m.empty, x) == x)
  }

  def apply[A](implicit monoid: Monoid[A]) = monoid
}

object AndMonoid {
  implicit val andMonoid: Monoid[Boolean] = new Monoid[Boolean] {
    def empty: Boolean = true
    def combine(x: Boolean, y: Boolean) = x && y
  }
}

object OrMonoid {
  implicit val orMonoid: Monoid[Boolean] = new Monoid[Boolean] {
    def empty: Boolean = false
    def combine(x: Boolean, y: Boolean) = x || y
  }
}

object EitherMonoid {
  implicit val eitherMonoid: Monoid[Boolean] = new Monoid[Boolean] {
    def empty = false
    def combine(x: Boolean, y: Boolean) = (x && !y) || (!x && y)
  }
}

object XnorMonoid {
  implicit val xnorMonoid: Monoid[Boolean] = new Monoid[Boolean] {
    def empty = true
    def combine(x: Boolean, y: Boolean) = (x || !y) && (!x || y)
  }
}

object SetUnionMonoid {
  implicit def setUnionMonoid[A]: Monoid[Set[A]] = new Monoid[Set[A]] {
    def empty: Set[A] = Set.empty[A]
    def combine(x: Set[A], y: Set[A]) = x union y
  }
}

object SetIntersectionSemigroup {
  implicit def setIntersectionSemigroup[A]: Semigroup[Set[A]] = new Semigroup[Set[A]] {
    def combine(x: Set[A], y: Set[A]) = x intersect y
  }
}

object SetSymDiffMonoid {
  implicit def setSymDiffMonoid[A]: Monoid[Set[A]] = new Monoid[Set[A]] {
    def empty: Set[A] = Set.empty[A]
    def combine(x: Set[A], y: Set[A]) = (x diff y) union (y diff x)
  }
}

object PlusMonoid {
  implicit val plusMonoid: Monoid[Int] = new Monoid[Int] {
    def combine(x: Int, y: Int) = x + y
    def empty = 0
  }
}

object MonoidTest extends App {
  import SetUnionMonoid._
  import PlusMonoid._

  val intMonoid = Monoid[Int]
  intMonoid.combine(1, 2)

  val intSetMonoid = Monoid[Set[Int]]
  intSetMonoid.combine(Set(1, 2), Set(2, 3))
}
