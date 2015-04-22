sealed trait Option[+A] {
  def map[B](f: A => B): Option[B] = this match {
    case None => None
    case Some(a) => Some(f(a))
  }

  def flatMap[B](f: A => Option[B]): Option[B] = this match {
    case None => None
    case Some(a) => f(a)
  }

  def getOrElse[B >: A](default: B): B = this match {
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
}

object ErrorTest extends App {
  import Option._
  println(variance(Seq(11,10)))
  println(variance2(Seq(11,10)))
}
