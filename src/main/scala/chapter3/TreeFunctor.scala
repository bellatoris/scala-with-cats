package chapter3

sealed trait Tree[+A]
final case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]
final case class Leaf[A](value: A) extends Tree[A]

object Tree {
  import cats.Functor
  implicit val treeFunctor: Functor[Tree] =
    new Functor[Tree] {
      def map[A, B](value: Tree[A])(func: A => B): Tree[B] = value match {
        case Branch(left, right) => Branch(map(left)(func), map(right)(func))
        case Leaf(value) => Leaf(func(value))
      }
    }

  import cats.Show

  implicit def treeShow[A](implicit showA: Show[A]): Show[Tree[A]] = {
    def nestedShow(tree: Tree[A]): String = tree match {
      case Branch(left, right) => {
        val leftBranch = nestedShow(left)
        val rightBranch = nestedShow(right)
        s"left is ${leftBranch}, right is ${rightBranch} "
      }
      case Leaf(value) => {
        showA.show(value)
      }
    }
    Show.show(nestedShow)
  }

  def branch[A](left: Tree[A], right: Tree[A]): Tree[A] =
    Branch(left, right)

  def leaf[A](value: A): Tree[A] = Leaf(value)
}

object TreeFunctorTest extends App {
  import cats.syntax.functor._
  import cats.syntax.show._
  import cats.instances.int._
  import Tree._

  println(Tree.leaf(100).map(_ * 2).show)
  println(Tree.branch(Tree.leaf(10), Tree.leaf(20)).map(_ * 2).show)
}
