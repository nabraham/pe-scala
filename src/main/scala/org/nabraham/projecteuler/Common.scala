package org.nabraham.projecteuler

/**
 * Created by nabraham on 8/10/15.
 */
object Common {
  val fibs: Stream[Int] = 0 #:: fibs.scanLeft(1)(_ + _)

  val primes = 2 #:: sieve(3)

  private def sieve(n: Int): Stream[Int] =
    if (primes.takeWhile(p => p * p <= n).exists(n % _ == 0)) sieve(n + 2)
    else n #:: sieve(n + 2)

  def input(filename: String): List[String] = {
    scala.io.Source.fromFile("input/" + filename).getLines.toList
  }
}
