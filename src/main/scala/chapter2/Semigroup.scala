package chapter2

trait Semigroup[A] {
  def combine(x: A, y: A): A
}
