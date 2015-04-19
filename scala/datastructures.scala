sealed trait List[+A]
case object Nil extends List[Nothing]
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {
  def sum(ints: List[Int]): Int = ints match {
    case Nil => 0
    case Cons(x, xs) => x + sum(xs)
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x, xs) => x * product(xs)
  }

  def apply[A](as: A*): List[A] =
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  def tail[A](l: List[A]): List[A] = l match {
    case Nil => Nil
    case Cons(x, xs) => xs
  }

  def setHead[A](l: List[A], x: A): List[A] = l match {
    case Nil => Nil
    case Cons(h, xs) => Cons(x, xs)
  }

  def drop[A](l: List[A], n: Int): List[A] = n match {
    case 0 => l
    case _ => drop(tail(l), n-1)
  }

  def dropWhile[A](l: List[A])(f: A => Boolean): List[A] = l match {
    case Nil => Nil
    case Cons(x,xs) => if (f(x)) dropWhile(xs)(f)
                       else Cons(x, dropWhile(xs)(f))
  }

  def append[A](a1: List[A], a2: List[A]): List[A] = a1 match {
    case Nil => a2
    case Cons(h,t) => Cons(h, append(t, a2))
  }

  def init[A](l: List[A]): List[A] = l match {
    case Nil => Nil
    case Cons(x,Nil) => Nil
    case Cons(x,xs) => Cons(x, init(xs))
  }

  def foldRight[A,B](as: List[A], z: B)(f: (A,B) => B): B = as match {
    case Nil => z
    case Cons(x, xs) => f(x, foldRight(xs, z)(f))
  }

  def sum2(ns: List[Int]) = foldRight(ns, 0)((x,y) => x + y)
  def product2(ns: List[Double]) = foldRight(ns, 1.0)((x,y) => x * y)

  def length[A](as: List[A]): Int = foldRight(as, 0)((_,l) => 1 + l)

  @annotation.tailrec
  def foldLeft[A,B](as: List[A], z: B)(f: (B,A) => B): B = as match {
    case Nil => z
    case Cons(x,xs) => foldLeft(xs, f(z,x))(f)
  }

  def reverse[A](as: List[A]): List[A] = foldLeft(as, Nil:List[A])((x,y) => Cons(y,x))

  def foldRight2[A,B](as: List[A], z: B)(f: (A,B) => B): B =
    foldLeft(reverse(as), z)((b,a) => f(a,b)) 
}

object Test {
  import List._

  def main(args: Array[String]) = {
    println(tail(List(1,2,3,4)))
    println(setHead(List(1,2,3,4), 5))
    println(drop(List(1,2,3,4), 2))
    println(dropWhile(List(1,2,3,4))(n => n % 2 == 0))
    println(init(List(1,2,3,4)))
    println(sum(List(1,2,3,4)))
    println(product(List(1,2,3,4)))
    println(sum2(List(1,2,3,4)))
    println(product2(List(1,2,3,4)))
    println(foldRight(List(1,2,3,4), Nil:List[Int])(Cons(_,_)))
    println(length(List(1,2,3,4)))
    println(foldLeft(List(1,2,3,4), Nil:List[Int])((x,y) => Cons(y,x)))
    println(reverse(List(1,2,3,4)))
  }
}
