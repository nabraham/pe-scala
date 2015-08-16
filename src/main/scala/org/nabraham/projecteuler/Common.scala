package org.nabraham.projecteuler

/**
 * Created by nabraham on 8/10/15.
 */
object Common {
  val fibs: Stream[Int] = 0 #:: fibs.scanLeft(1)(_ + _)
  val bigFibs: Stream[BigInt] = BigInt(0) #:: bigFibs.scanLeft(BigInt(1))(_ + _)

  val primes = 2 #:: sieve(3)

  private def sieve(n: Int): Stream[Int] =
    if (primes.takeWhile(p => p * p <= n).exists(n % _ == 0)) sieve(n + 2)
    else n #:: sieve(n + 2)

  def input(filename: String): List[String] = {
    scala.io.Source.fromFile("input/" + filename).getLines.toList
  }

  private def splitLine(line: String) : (String, String) = {
    val parts = line.split(" ").map(_.trim)
    if (parts.length == 2) {
      (parts(0).replace(".", ""), parts(1))
    } else {
      ("", "")
    }

  }
  private val solutionMap = scala.io.Source.fromFile("solutions.txt").getLines.map(splitLine).toList.groupBy(_._1).mapValues(_.map(_._2))
  def solutions(problem: String): String = {
    solutionMap(problem).head
  }
}
