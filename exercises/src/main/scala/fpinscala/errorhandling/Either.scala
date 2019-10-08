package fpinscala.errorhandling


import scala.{Option => _, Either => _, Left => _, Right => _, _} // hide std library `Option` and `Either`, since we are writing our own in this chapter

sealed trait Either[+E,+A] {
 def map[B](f: A => B): Either[E, B] = this match {
   case Right(v) => Right(f(v))
   case Left(e) => Left(e)
 }

 def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B] = this match {
   case Right(v) => f(v)
   case Left(e) => Left(e)
 }

 def orElse[EE >: E, B >: A](b: => Either[EE, B]): Either[EE, B] = this match {
   case Right(v) => Right(v)
   case Left(_) => b
 }

 def map2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C] =
   this flatMap(a => b map(b => f(a,b)))
}
case class Left[+E](get: E) extends Either[E,Nothing]
case class Right[+A](get: A) extends Either[Nothing,A]

object Either {
  def traverse[E,A,B](es: List[A])(f: A => Either[E, B]): Either[E, List[B]] = es match {
    case Nil => Right(Nil)
    case h :: t => for {
      a <- f(h)
      b <- traverse(t)(f)
    } yield (a :: b)
  }

  def sequence[E,A](es: List[Either[E,A]]): Either[E,List[A]] = es match {
    case Nil => Right(Nil)
    case h :: t => for {
      a <- h
      b <- sequence(t)
    } yield (a :: b)
  }

  def mean(xs: IndexedSeq[Double]): Either[String, Double] = 
    if (xs.isEmpty) 
      Left("mean of empty list!")
    else 
      Right(xs.sum / xs.length)

  def safeDiv(x: Int, y: Int): Either[Exception, Int] = 
    try Right(x / y)
    catch { case e: Exception => Left(e) }

  def Try[A](a: => A): Either[Exception, A] =
    try Right(a)
    catch { case e: Exception => Left(e) }

}

object EitherTest {
  import Either._

  def operation(v: Int): Either[String, Int] = {
    if (v > 3) Right(v + 3) else Left("lower than 4")
  }

  def main (args: Array[String] ): Unit = {
    val a = Right(7).map(v => v + 1)
    val a2 = Left("error").map(v => "oj")

    val b = Right(7).flatMap(operation)
    val b2 = Right(2).flatMap(operation)
    val b3 = Left("my custom error").flatMap(operation)
    val b4 = Right(2).map(operation)

    val c = Right(7).orElse(Right(3))
    val c1 = Left("x").orElse(Right(3))
    val c2 = Left("x").orElse(Left("y"))

    val d = Right(5).map2(Right(3))((a, b) => a + b)
    val d1 = Left("x").map2(Right(3))((a: Int, b: Int) => a + b)
    val d2 = Right(5).map2(Left("y"))((a: Int, b: Int) => a + b)

    val e = traverse(List(1, 2, 3, 4))(x => Right(x+1))
    val e1 = traverse(List(1, 2, 3, 0))(x => if(x != 0) Right(1/x) else Left("Division by 0"))

    val f = sequence(List(Right(2), Right(3), Right(4)))
    val f1 = sequence(List(Left(1), Left(3), Right(4)))

    println(e)
    println(e1)
    //println(d2)

  }
}