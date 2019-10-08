package fpinscala.laziness

import Stream._
trait Stream[+A] {

  def foldRight[B](z: => B)(f: (A, => B) => B): B = // The arrow `=>` in front of the argument type `B` means that the function `f` takes its second argument by name and may choose not to evaluate it.
    this match {
      case Cons(h,t) => f(h(), t().foldRight(z)(f)) // If `f` doesn't evaluate its second argument, the recursion never occurs.
      case _ => z
    }

  def exists(p: A => Boolean): Boolean = 
    foldRight(false)((a, b) => p(a) || b) // Here `b` is the unevaluated recursive step that folds the tail of the stream. If `p(a)` returns `true`, `b` will never be evaluated and the computation terminates early.

  @annotation.tailrec
  final def find(f: A => Boolean): Option[A] = this match {
    case Empty => None
    case Cons(h, t) => if (f(h())) Some(h()) else t().find(f)
  }
  def take(n: Int): Stream[A] = (this, n) match {
    case (Cons(h, t), n) => cons(h(), t().take(n - 1))
    case _ => Empty
  }

  def drop(n: Int): Stream[A] = ???

  def takeWhile(p: A => Boolean): Stream[A] = this match {
    case Cons(h, t) if p(h()) => cons(h(), t().takeWhile(p))
    case _ => Empty
  }

  def takeWhile2(p: A => Boolean): Stream[A] =
    foldRight(empty[A])((h, t) => if(p(h)) cons(h, t) else empty)

  def forAll(p: A => Boolean): Boolean =
    foldRight(true)((h, t) => p(h) && t)

  def headOption: Option[A] = foldRight(None: Option[A])((h, t) => Some(h))

  // 5.7 map, filter, append, flatmap using foldRight. Part of the exercise is
  // writing your own function signatures.

  def map[B](f: A => B): Stream[B] = foldRight(empty: Stream[B])((h, t) => cons(f(h), t))
  def filter(f: A => Boolean): Stream[A] = foldRight(empty: Stream[A])((h, t) => if(f(h)) cons(h, t) else t)
  def append[B>:A](e: Stream[B]): Stream[B] = foldRight(e)((h, t) => cons(h, t))
  def flatMap[B](f: A => Stream[B]): Stream[B] =
    foldRight(empty[B])((h,t) => f(h) append t)

  def startsWith[B](s: Stream[B]): Boolean = ???

  def toList: List[A] = this match {
    case Empty => Nil
    case Cons(h, t) => h() :: t().toList
  }

  def mapUnfold[B](f: A => B): Stream[B] = unfold(this) {
    case Cons(h, t) => Some((f(h()), t()))
    case _ => None
  }

  def takeUnfold(n: Int): Stream[A] =
    unfold((this,n)) {
      case (Cons(h,t), 1) => Some((h(), (empty, 0)))
      case (Cons(h,t), n) if n > 1 => Some((h(), (t(), n-1)))
      case _ => None
    }

  def takeWhileUnfold(p: A => Boolean): Stream[A] =
    unfold(this) {
      case Cons(h,t) if p(h()) => Some((h(), t()))
      case _ => None
    }

  def zipWith[B,C](b: Stream[B])(f: (A, B) => C): Stream[C] = unfold((this, b)) {
    case (Cons(h1, t1), Cons(h2, t2)) => Some((f(h1(), h2()), (t1(), t2())))
    case _ => None
  }

  def zipAll[B](s2: Stream[B]): Stream[(Option[A],Option[B])] =
    zipWithAll(s2)((_,_))

  def zipWithAll[B, C](s2: Stream[B])(f: (Option[A], Option[B]) => C): Stream[C] =
    Stream.unfold((this, s2)) {
      case (Empty, Empty) => None
      case (Cons(h, t), Empty) => Some(f(Some(h()), Option.empty[B]) -> (t(), empty[B]))
      case (Empty, Cons(h, t)) => Some(f(Option.empty[A], Some(h())) -> (empty[A] -> t()))
      case (Cons(h1, t1), Cons(h2, t2)) => Some(f(Some(h1()), Some(h2())) -> (t1() -> t2()))
    }

  //def hasSubsequence(s2: Stream): Boolean = unfold()
}
case object Empty extends Stream[Nothing]
case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {
  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }

  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) empty 
    else cons(as.head, apply(as.tail: _*))

  val ones: Stream[Int] = Stream.cons(1, ones)
  def constant[A](a: A): Stream[A] = Stream.cons(a, constant(a))
  def from(n: Int): Stream[Int] = Stream.cons(n, from(n + 1))
//n: 3
//3, 4, 5, 6

  // 0, 1, 1, 2, 3, 5, 8
  def fib: Stream[Int] = {
    def next(prev: Int, cur: Int): Stream[Int] = Stream.cons(prev, next(cur, prev + cur))

    next(0, 1)
  }

  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] =
    f(z) match {
      case Some((h,s)) => cons(h, unfold(s)(f))
      case None => empty
    }

  val onesUnfold: Stream[Int] = unfold(1)(x => Some((1,1)))
  def constantUnfold[A](a: A): Stream[A] = unfold(a)(x => Some(x, x))
  def fibUnfold: Stream[Int] = unfold((0, 1))(x => Some(x._1, (x._2, x._1 + x._2)))
  def fromUnfold(n: Int): Stream[Int] = unfold(n)(x => Some(x, x + 1))
}

object StreamTest {
  import Stream._

  def main(args: Array[String]): Unit = {
    val a = cons(7, cons(5, cons(3, Empty)))
    val b = a.take(1).toList
    val c = cons(7, cons(5, cons(3, Empty))).takeWhile(x => x > 4).toList
    val d = a.forAll(x => x > 0);
    val e = cons(7, cons(5, cons(3, Empty))).takeWhile2(x => x > 4).toList
    val f = a.headOption
    val g = Empty.headOption
    val h = a.map(x => x + 2).toList
    val i = a.filter(x => x == 7 || x == 3).toList
    val j = a.append(cons(9, Empty)).toList
    val k = Stream.constant("w").take(5).toList
    val l = Stream.from(7).take(10).toList
    val m = Stream.fib.take(8).toList
    val n = Stream.onesUnfold.take(5).toList
    val o = Stream.constantUnfold(4).take(5).toList
    val p = Stream.fibUnfold.take(8).toList
    val r = Stream.fromUnfold(5).take(5).toList
    val s = Stream.from(3).take(5).mapUnfold(x => x + 1).toList
    val t = Stream.from(1).takeUnfold(5).toList
    val u = Stream.from(1).takeWhileUnfold(x => x < 5).toList
    val v = Stream.from(1).zipWith(Stream.from(1))((x1, x2) => x1 + x2).take(5).toList
    val x = Stream.from(5).take(4).zipAll(Stream.ones.take(5)).take(7).toList
    //println(Stream.ones.toList)
    println(x)

  }
}