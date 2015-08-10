/**
  * Created by nabraham on 8/8/15.
  */
import Common.fibs
import Common.primes

object Problems {

  //Find the sum of all the multiples of 3 or 5 below 1000.
  def p001 (): Unit = {
    val rez = (3 until 1000).filter(x => x % 3 == 0 || x % 5 == 0).sum
    assert(rez == 233168)
  }

  //By considering the terms in the Fibonacci sequence whose values do not exceed four million,
  //find the sum of the even-valued terms
  def p002 (): Unit = {
    val rez = fibs.takeWhile(_ < 4000000).filter(_ % 2 == 0).sum
    assert(rez == 4613732)
  }

  //What is the largest prime factor of the number 600851475143 ?
  def p003 (): Unit = {
    def recurDivide(n: Long, factor: Int): Long = {
      if (n % factor == 0) { recurDivide(n / factor, factor) }
      else { n }
    }

    def factorize(n: Long, factor: Int, largest: Int): Int = {
      if (factor > n) { largest }
      else if (n % factor == 0) { factorize(recurDivide(n, factor), factor + 1, factor) }
      else { factorize(n, factor + 1, largest) }
    }

    val rez = factorize(600851475143L, 2, 1)
    assert(rez == 6857)
  }

  //Find the largest palindrome made from the product of two 3-digit numbers.
  def p004 (): Unit = {
    val nums = (100 to 999)
    val rez = nums.flatMap(x => nums.map(y => y * x)).filter(z => z.toString == z.toString.reverse).max
    assert(rez == 906609)
  }

  //What is the smallest number divisible by each of the numbers 1 to 20?*
  def p005 (): Unit = {
    def remainder(n: Int, factors: List[Int]): Int = {
      if (n == 1 || factors.isEmpty) { n }
      else if (n % factors.head == 0) { remainder(n / factors.head, factors.tail) }
      else { remainder(n, factors.tail) }
    }

    def requiredFactors(factors: List[Int], n: Int): List[Int] = {
      remainder(n, factors) :: factors
    }

    val rez = (2 to 20).foldLeft(List(1))(requiredFactors).product
    assert(rez == 232792560)
  }

  //Find the difference between the sum of the squares of the first one hundred natural numbers and the square of the sum.
  def p006(): Unit = {
    val sumSquares = (1 to 100).map(x => x * x).sum
    val squareSums = Math.pow((1 to 100).sum, 2).toInt
    assert(squareSums - sumSquares == 25164150)
  }

  //What is the 10 001st prime number?
  def p007(): Unit = {
    assert(primes(10000) == 104743)
  }

}
