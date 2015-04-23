sealed trait Option[+A] {
  def map[B](f: A => B): Option[B] = this match {
    case None => None
    case Some(a) => Some(f(a))
  }

  def flatMap[B](f: A => Option[B]): Option[B] = this match {
    case None => None
    case Some(a) => f(a)
  }

  def getOrElse[B >: A](default: => B): B = this match {
    case None => default
    case Some(a) => a
  }

  def orElse[B >: A](ob: Option[B]): Option[B] = this match {
    case None => ob
    case _ => this
  }

  def filter(f: A => Boolean): Option[A] = this match {
    case Some(a) if f(a) => this
    case _ => None
  }
}

case class Some[B](get: B) extends Option[B]
case object None extends Option[Nothing]

object Option {
  def mean(xs: Seq[Double]): Option[Double] =
    if (xs.isEmpty) None
    else Some(xs.sum / xs.length)

  def variance(xs: Seq[Double]): Option[Double] =
    mean(xs) flatMap (m => mean(xs.map(x => math.pow(x - m, 2))))

  def variance2(xs: Seq[Double]): Option[Double] =
    for {
      m <- mean(xs)
      v <- mean(xs.map(x => math.pow(x - m, 2)))
    } yield v

  def Try[A](a: => A): Option[A] =
    try Some(a)
    catch { case e: Exception => None }

  def map2[A,B,C](a: Option[A], b: Option[B])(f: (A,B) => C): Option[C] =
    for {
      a1 <- a
      b1 <- b
    } yield f(a1,b1)

  def sequence[A](as: List[Option[A]]): Option[List[A]] =
    as match {
      case Nil => Some(Nil)
      case Cons(h,t) => for {
        a <- h
        tt <- sequence(t)
      } yield Cons(a, tt)
    }

  def sequence2[A](as: List[Option[A]]): Option[List[A]] =
    as match {
      case Nil => Some(Nil)
      case Cons(h,t) => h.flatMap(a => sequence(t).map(Cons(a, _)))
    }

  def traverse[A,B](as: List[A])(f: A => Option[B]): Option[List[B]] =
    sequence(List.map(as)(f))

  def traverse2[A,B](as: List[A])(f: A => Option[B]): Option[List[B]] =
    as match {
      case Nil => Some(Nil)
      case Cons(h,t) => for {
        hh <- f(h)
        tt <- traverse2(t)(f)
      } yield Cons(hh, tt)
    }

  def sequence3[A](as: List[Option[A]]): Option[List[A]] =
    traverse2(as)(a => a)
}

object ErrorTest extends App {
  import Option._
  println(variance(Seq(11,10)))
  println(variance2(Seq(11,10)))
  println(sequence(List(Some(1), Some(2))))
  println(sequence(List(Some(1), None)))
  println(sequence2(List(Some(1), Some(2))))
  println(sequence2(List(Some(1), None)))
  println(traverse(List("1","2"))(s => Try(s.toInt)))
  println(traverse(List("1","a"))(s => Try(s.toInt)))
  println(traverse2(List("1","2"))(s => Try(s.toInt)))
  println(traverse2(List("1","a"))(s => Try(s.toInt)))
}
