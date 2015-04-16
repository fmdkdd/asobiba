object MyModule {
  def fibo(n: Int): Int = {
    @annotation.tailrec
    def go(i: Int, prev: Int, cur: Int): Int =
      if (i == 0) prev
      else go(i-1, cur, prev + cur)
    go(n, 0, 1)
  }

  def factorial(n: Int): Int = {
    @annotation.tailrec
    def go(n: Int, acc: Int): Int =
      if (n == 0) acc
      else go(n-1, n*acc)
    go(n, 1)
  }

  def findFirst[A](as: Array[A], p: A => Boolean): Int = {
    @annotation.tailrec
    def loop(n: Int): Int =
      if (n >= as.length) -1
      else if (p(as(n))) n
      else loop(n+1)
    loop(0)
  }

  def isSorted[A](as: Array[A], ordered: (A,A) => Boolean): Boolean = {
    def loop(n: Int): Boolean =
      if (n >= as.length) true
      else if (ordered(as(n-1), as(n))) loop(n+1)
      else false
    loop(1)
  }


  def abs(n : Int): Int =
    if (n < 0) -n
    else n

  private def formatAbs(x: Int) = {
    val msg = "The absolute value of %d is %d"
    msg.format(x, abs(x))
  }

  def main(args: Array[String]): Unit =
    println(formatAbs(-22))
}
