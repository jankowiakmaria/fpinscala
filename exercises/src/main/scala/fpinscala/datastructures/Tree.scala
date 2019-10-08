package fpinscala.datastructures

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]


object Tree {
  def size[A](t: Tree[A]): Int = t match {
    case l: Leaf[A] => 1
    case b: Branch[A] => size(b.left) + size(b.right) + 1
  }

  def maximum(t: Tree[Int]): Int = t match {
    case Leaf(l) => l
    case Branch(l, r) => maximum(l).max(maximum(r))
  }

  def depth[A](t: Tree[A]): Int = t match {
    case Leaf(_) => 0
    case Branch(l, r) => 1 + (depth(l) max depth(r))
  }

  def map[A, B](t: Tree[A])(f: A => B): Tree[B] = t match {
    case Leaf(l) => Leaf(f(l))
    case Branch(l, r) => Branch(map(l)(f), map(r)(f))
  }

  def fold[A, B, C](t: Tree[A])(g: A => B)(f: (B, B) => B): B = t match {
    case Leaf(l) => g(l)
    case Branch(l, r) => f(fold(l)(g)(f), fold(r)(g)(f))
  }

  def size2[A](t: Tree[A]): Int = fold(t)(_ => 1)((x, y) => x + y + 1)

  def maximum2(t: Tree[Int]): Int = fold(t)(l => l)((x, y) => x.max(y))

  def depth2[A](t: Tree[A]): Int = fold(t)(_ => 0)((x, y) => x.max(y) + 1)

  def map2[A, B](t: Tree[A])(f: A => B): Tree[B] = fold(t)(l => Leaf(f(l)): Tree[B])((x: Tree[B], y: Tree[B]) => Branch(x, y))
}

object TreeTest {
  import Tree._

  def main(args: Array[String]): Unit = {
    val tree = Branch(Leaf(2), Branch(Leaf(6), Leaf(3)))
    val a = size(tree)
    val b = maximum(tree)
    val c = depth(tree)
    val d = map(tree)(l => l + 1)

    val e = size2(tree)
    val f = maximum2(tree)
    val g = depth2(tree)
    val h = map2(tree)(l => l + 1)

    println(d)
    println(h);
  }

}