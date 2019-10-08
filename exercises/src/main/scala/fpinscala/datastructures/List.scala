package fpinscala.datastructures

sealed trait List[+A] // `List` data type, parameterized on a type, `A`
case object Nil extends List[Nothing] // A `List` data constructor representing the empty list
/* Another data constructor, representing nonempty lists. Note that `tail` is another `List[A]`,
which may be `Nil` or another `Cons`.
 */
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List { // `List` companion object. Contains functions for creating and working with lists.
  def sum(ints: List[Int]): Int = ints match { // A function that uses pattern matching to add up a list of integers
    case Nil => 0 // The sum of the empty list is 0.
    case Cons(x,xs) => x + sum(xs) // The sum of a list starting with `x` is `x` plus the sum of the rest of the list.
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x,xs) => x * product(xs)
  }

  def apply[A](as: A*): List[A] = // Variadic function syntax
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  val x = List(1,2,3,4,5) match {
    case Cons(x, Cons(2, Cons(4, _))) => x
    case Nil => 42
    case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
    case Cons(h, t) => h + sum(t)
    case _ => 101
  }

  def append[A](a1: List[A], a2: List[A]): List[A] =
    a1 match {
      case Nil => a2
      case Cons(h,t) => Cons(h, append(t, a2))
    }

  def foldRight[A,B](as: List[A], z: B)(f: (A, B) => B): B = // Utility functions
    as match {
      case Nil => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }

  def sum2(ns: List[Int]) =
    foldRight(ns, 0)((x,y) => x + y)

  def product2(ns: List[Double]) =
    foldRight(ns, 1.0)(_ * _) // `_ * _` is more concise notation for `(x,y) => x * y`; see sidebar

  def tail[A](l: List[A]): List[A] = {
    l match {
      case Nil => Nil
      case Cons(_, t) => t
    }
  }

  def setHead[A](l: List[A], h: A): List[A] = {
    l match {
      case Nil => Nil
      case Cons(_, t) => Cons(h, t)
    }
  }

  def drop[A](l: List[A], n: Int): List[A] = {
    if(n <= 0) l
    else
      l match {
        case Nil => Nil
        case Cons(_, t) => drop(t, n - 1)
      }
  }

  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = {
    l match {
      case Nil => Nil
      case Cons(h, t) => {
        if(!f(h)) l
        else dropWhile(t, f)
      }
    }
  }

  def init[A](l: List[A]): List[A] = {
    l match {
      case Nil => Nil
      case Cons(_, Nil) => Nil
      case Cons(h, t) => Cons(h, init(t))
    }
  }

  def length[A](l: List[A]): Int = {
    foldRight(l, 0)((_, x) => 1 + x)
  }

  @annotation.tailrec
  def foldLeft[A,B](l: List[A], z: B)(f: (B, A) => B): B = {
    l match {
      case Nil => z
      case Cons(h, xs) => foldLeft(xs, f(z, h))(f)
    }
  }

  def sum3(ns: List[Int]) =
    foldLeft(ns, 0)((x,y) => x + y)

  def product3(ns: List[Double]) =
    foldLeft(ns, 1.0)(_ * _)

  def length2[A](l: List[A]): Int = {
    foldLeft(l, 0)((x, h) => 1 + x)
  }

  def reverse[A](l: List[A]): List[A] = {
    foldLeft(l, List[A]())((x, y) => Cons(y, x))
  }

  def append3[A](a1: List[A], a2: List[A]): List[A] =
    foldRight(a1, a2)((as2,as1) => Cons(as2, as1))

  def concatenate[A](a: List[List[A]]): List[A] = {
    a match {
      case Nil => Nil
      case Cons(h, t) => append(h, concatenate(t))
    }
  }

  def add1(l: List[Int]): List[Int] = l match {
    case Nil => Nil
    case Cons(h, t) => Cons(h+1, add1(t))
  }

  def add1_2(l:List[Int]): List[Int] =
    foldRight(l, Nil: List[Int])((x: Int, y: List[Int]) => Cons(x+1, y))

  def doubleToString(l: List[Double]): List[String] =
    foldRight(l, Nil: List[String])((h, ts) => Cons(h.toString, ts))

  def map[A,B](l: List[A])(f: A => B): List[B] =
    foldRight(l, Nil: List[B])((h, t) => Cons(f(h), t))

  def filter[A](l: List[A])(f: A => Boolean): List[A] = {
    foldRight(l, Nil: List[A])((h, t) => {
      if (f(h)) Cons(h, t)
      else t
    })
  }

  def flatMap[A, B](l: List[A])(f: A => List[B]): List[B] =
    foldRight(l, Nil: List[B])((h, t) => append(f(h), t))

  def flatMapFilter[A](l: List[A])(f: A => Boolean): List[A] =
    flatMap(l)(x => if (f(x)) Cons(x, Nil) else List[A]())

  def addLists(a1: List[Int], a2: List[Int]): List[Int] = (a1, a2) match {
    case (Nil, _) => Nil
    case (_, Nil) => Nil
    case(Cons(h1, t1), Cons(h2, t2)) => Cons(h1 + h2, addLists(t1, t2))
  }

  def zipWith[A](a1: List[A], a2: List[A])(f: (A, A) => A): List[A] = (a1, a2) match {
    case (Nil, _) => Nil
    case (_, Nil) => Nil
    case(Cons(h1, t1), Cons(h2, t2)) => Cons(f(h1, h2), zipWith(t1, t2)(f))
  }

  def hasSubsequence[A](sup: List[A], sub: List[A]): Boolean = {
    (sup, sub) match {
      case (Nil, Nil) => true
      case (Nil, _) => false
      case (_, Nil) => true
      case (Cons(h1, t1), Cons(h2, t2)) if (h1 == h2) => hasSubsequence(t1, t2)
      case (Cons(h1, t1), _) => hasSubsequence(t1, sub)
    }
  }
}

object ListTest {
  import List._

  def main(args: Array[String]): Unit = {
    val x = List(1,2,3,4,5) match {
      case Cons(x, Cons(2, Cons(4, _))) => x
      case Nil => 42
      case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
      case Cons(h, t) => h + sum(t)
      case _ => 101
    }

    val y = init(List(1,2,3,4))
    val z = foldRight(List(1,2,3), Nil: List[Int])(Cons(_,_))
    val a = reverse(List(1,2,3,4))
    val b = reverse(List(Nil))
    val c = append3(List(1,2,3), List(4,5,6))
    val d = concatenate(List(List(1,2,3), List(4,5,6)))
    val e = add1(List(1,2,3))
    val f = add1_2(List(1,2,3))
    val g = doubleToString(List(1.2, 3.4, 5.6))
    val h = map(List(1,2,3))(x => x + 1);
    val i = map(List(1.2, 3.4, 5.6))(x => x.toString)
    val j = filter(List(1,2,3,4,5,6))(x => x%2 == 0)
    val k = flatMap(List(1,2,3,4))(i => List(i,i))
    val l = flatMapFilter(List(1,2,3,4,5,6))(x => x%2 == 0)
    val m = addLists(List(1,2,3), List(4,5, 6))
    val n = zipWith(List(1,2,3), List(4,5,6))(_ + _)
    val o = hasSubsequence(List(1,2,3,4), List(3,4))

    println(o);
  }
}