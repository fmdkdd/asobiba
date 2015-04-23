object Invariance {
  // Why do you need to state [B >: A] in the signatures of getOrElse and orElse?
  // Let's start with the straightforward case of using the same type A
  // everywhere.

  sealed trait OptionInvariant[A] {
    def getOrElse(default: => A): A = this match {
      case None() => default
      case Some(a) => a
    }

    def getOrElse2[B >: A](default: => B): B = this match {
      case None() => default
      case Some(a) => a
    }
  }

  // We could define orElse inside Some and None as well.
  case class Some[A](get: A) extends OptionInvariant[A] {
    // def orElse(ob: OptionInvariant[A]): OptionInvariant[A] = this
  }

  // We have to make None take a type A, since if it were an
  // OptionInvariant[Nothing], we would not be able to type `mean` without
  // resorting to covariance.
  case class None[A]() extends OptionInvariant[A] {
    // def orElse(ob: OptionInvariant[A]): OptionInvariant[A] = ob
  }

  def mean(xs: Seq[Double]): OptionInvariant[Double] =
    if (xs.isEmpty) None()
    else Some(xs.sum / xs.length)

  def main(args: Array[String]) = {
    // You can do
    println(mean(Seq()).getOrElse(1))
    // But not
    // println(mean(Seq()).getOrElse("Nope!"))
    // With getOrElse2, which is more general, I can.
    println(mean(Seq()).getOrElse2("Nope!"))
  }
}

object Covariance {
  // Now I make A covariant by writing +A.
  sealed trait OptionCovariant[+A] {
    // Then I cannot define getOrElse to be invariant.  Why?  Because arguments
    // are contravariant: an overridden getOrElse should be able to accept
    // supertypes of A as arguments without loss of safety.  But here we are
    // saying that the argument /must/ be contravariant.
    // def getOrElse(default: => A): A = this match {
    //   case None() => default
    //   case Some(a) => a
    // }

    // So we have to explicitly state that we want contravariance back.
    def getOrElse2[B >: A](default: => B): B = this match {
      case None() => default
      case Some(a) => a
    }
  }

  case class Some[A](get: A) extends OptionCovariant[A]
  case class None[A]() extends OptionCovariant[A]

  def mean(xs: Seq[Double]): OptionCovariant[Double] =
    if (xs.isEmpty) None()
    else Some(xs.sum / xs.length)
}
