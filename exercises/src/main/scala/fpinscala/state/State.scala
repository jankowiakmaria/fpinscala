package fpinscala.state


trait RNG {
  def nextInt: (Int, RNG) // Should generate a random `Int`. We'll later define other functions in terms of `nextInt`.
}

object RNG {
  // NB - this was called SimpleRNG in the book text

  case class Simple(seed: Long) extends RNG {
    def nextInt: (Int, RNG) = {
      val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL // `&` is bitwise AND. We use the current seed to generate a new seed.
      val nextRNG = Simple(newSeed) // The next state, which is an `RNG` instance created from the new seed.
      val n = (newSeed >>> 16).toInt // `>>>` is right binary shift with zero fill. The value `n` is our new pseudo-random integer.
      (n, nextRNG) // The return value is a tuple containing both a pseudo-random integer and the next `RNG` state.
    }
  }

  type Rand[+A] = RNG => (A, RNG)

  val int: Rand[Int] = _.nextInt

  def unit[A](a: A): Rand[A] =
    rng => (a, rng)

  def map[A,B](s: Rand[A])(f: A => B): Rand[B] =
    rng => {
      val (a, rng2) = s(rng)
      (f(a), rng2)
    }

  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    val (x, rng2) = rng.nextInt
    x match {
      case Int.MinValue => (0, rng2)
      case _ => (Math.abs(x), rng2)
    }
  }

  def double(rng: RNG): (Double, RNG) = {
    val (x, rng2) = nonNegativeInt(rng)
    val y = Math.abs(x - 1)
    val d = (y.toDouble/Int.MaxValue.toDouble)
    (d, rng2)
  }

  def double_re: Rand[Double] = {
    map(nonNegativeInt)(x => x.toDouble/(Int.MaxValue + 1).toDouble)
  }

  def intDouble(rng: RNG): ((Int,Double), RNG) = {
    val (one, rng2) = rng.nextInt
    val (two, rng3) = double(rng2)
    ((one, two), rng3)
  }

  def doubleInt(rng: RNG): ((Double,Int), RNG) = {
    val (one, rng2) = double(rng)
    val (two, rng3) = rng2.nextInt
    ((one, two), rng3)
  }

  def double3(rng: RNG): ((Double,Double,Double), RNG) = {
    val (one, rng2) = double(rng)
    val (two, rng3) = double(rng2)
    val (three, rng4) = double(rng3)
    ((one, two, three), rng4)
  }

  def ints(count: Int)(rng: RNG): (List[Int], RNG) = {
    def go(count: Int, list: List[Int], rng: RNG): (List[Int], RNG) = {
      if(count == 0) (list, rng)
      else {
        val (r, rng2) = rng.nextInt
        go(count - 1, r :: list, rng2)
      }
    }

    go(count, Nil, rng)
  }

  def map2[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = ???

  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] = ???

  def flatMap[A,B](f: Rand[A])(g: A => Rand[B]): Rand[B] = ???
}

case class State[S,+A](run: S => (A, S)) {
  def map[B](f: A => B): State[S, B] =
    ???
  def map2[B,C](sb: State[S, B])(f: (A, B) => C): State[S, C] =
    ???
  def flatMap[B](f: A => State[S, B]): State[S, B] =
    ???
}

sealed trait Input
case object Coin extends Input
case object Turn extends Input

case class Machine(locked: Boolean, candies: Int, coins: Int)

object State {
  type Rand[A] = State[RNG, A]
  def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] = ???
}

object StateTest{
  import RNG._

  def main(args: Array[String]): Unit = {
    val rng = Simple(123)
    val a = nonNegativeInt(rng)
    val b = double(rng)
    val c = ints(3)(rng)
    val d = double_re(rng)


    println(d)
  }
}