package org.nabraham.projecteuler

import Common.fibs
import Common.input
import Common.primes

/**
 * Created by nabraham on 8/10/15.
 */
object Problems {

  //Find the sum of all the multiples of 3 or 5 below 1000.
  def p001(): Unit = {
    val rez = (3 until 1000).filter(x => x % 3 == 0 || x % 5 == 0).sum
    assert(rez == 233168)
  }

  //By considering the terms in the Fibonacci sequence whose values do not exceed four million,
  //find the sum of the even-valued terms
  def p002(): Unit = {
    val rez = fibs.takeWhile(_ < 4000000).filter(_ % 2 == 0).sum
    assert(rez == 4613732)
  }

  //What is the largest prime factor of the number 600851475143 ?
  def p003(): Unit = {
    def recurDivide(n: Long, factor: Int): Long = {
      if (n % factor == 0) {
        recurDivide(n / factor, factor)
      }
      else {
        n
      }
    }

    def factorize(n: Long, factor: Int, largest: Int): Int = {
      if (factor > n) {
        largest
      }
      else if (n % factor == 0) {
        factorize(recurDivide(n, factor), factor + 1, factor)
      }
      else {
        factorize(n, factor + 1, largest)
      }
    }

    val rez = factorize(600851475143L, 2, 1)
    assert(rez == 6857)
  }

  //Find the largest palindrome made from the product of two 3-digit numbers.
  def p004(): Unit = {
    val nums = (100 to 999)
    val rez = nums.flatMap(x => nums.map(y => y * x)).filter(z => z.toString == z.toString.reverse).max
    assert(rez == 906609)
  }

  //What is the smallest number divisible by each of the numbers 1 to 20?*
  def p005(): Unit = {
    def remainder(n: Int, factors: List[Int]): Int = {
      if (n == 1 || factors.isEmpty) {
        n
      }
      else if (n % factors.head == 0) {
        remainder(n / factors.head, factors.tail)
      }
      else {
        remainder(n, factors.tail)
      }
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

  //Find the thirteen adjacent digits in the 1000-digit number that have the
  //greatest product. What is the value of this product?
  def p008(): Unit = {
    val rez = input("008.txt").flatMap(_.trim).map(_.toString.toInt).sliding(4).map(_.product).max
    assert(rez == 5832)
  }

  //There exists exactly one Pythagorean triplet for which a + b + c = 1000.
  //Find the product abc.
  def p009(): Unit = {
    def sq(x: Int): Int = {
      x * x
    }
    val nums = (1 until 1000)
    val rez = for (a <- nums; b <- 1 to a; c = 1000 - (a + b) if sq(a) + sq(b) == sq(c)) yield a * b * c
    assert(rez.head == 31875000)
  }

  //Find the sum of all the primes below two million.
  def p010(): Unit = {
    val rez = primes.takeWhile(_ < 2000000).foldLeft(0L)(_ + _) //fold left instead of sum bc of MAX_INT
    assert(rez == 142913828922L)
  }

  //What is the greatest product of four adjacent numbers in the same direction (up, down, left, right, or diagonally)
  //in the 20×20 grid?
  def p011(): Unit = {
    val rows = input("011.txt").map(_.trim.split(" ").toList.map(_.toInt))
    val size = rows.length
    val cols = (0 until size).map(i => rows.map(_(i)))

    def diags(rows: Iterable[Iterable[Int]]): Iterable[Iterable[Int]] = {
      val zippedRows = rows.map(_.zipWithIndex).zipWithIndex
      val diagIndexed = zippedRows.map(rz => rz._1.map(cz => (cz._1, cz._2 - rz._2))).flatten
      (-size + 1 until size).map(i => diagIndexed.filter(_._2 == i).map(_._1))
    }

    val backSlash = diags(rows)
    val forwardSlash = diags(rows.map(_.reverse))
    val rez = List(rows, cols, backSlash, forwardSlash).flatMap(_.map(_.sliding(4).toList.map(_.product))).flatten.max
    assert(rez == 70600674)
  }

  //What is the value of the first triangle number to have over five hundred divisors?
  def p012(): Unit = {
    def triangle(n: Int): Int = {
      (n * n + n) / 2
    }

    //https://pavelfatin.com/scala-for-project-euler
    def ndivisors(t: Int): Int = Range(1, Int.MaxValue)
      .takeWhile(n => n * n <= t)
      .foldLeft(0)((s, n) => if (t % n == 0) s + 2 else s)

    val num = (501 to Int.MaxValue).find(x => ndivisors(triangle(x)) > 500).get
    assert(triangle(num) == 76576500)
  }

  //Work out the first ten digits of the sum of the following one-hundred 50-digit numbers.
  def p013(): Unit = {
    val rez = input("013.txt").map(BigInt(_)).sum.toString.substring(0, 10)
    assert(rez == "5537376230")
  }

  //Which starting number, under one million, produces the longest chain?
  def p014(): Unit = {
    def collatz(n: Long, count: Int): Int = {
      if (n <= 1) {
        count
      } else if (n % 2 == 0) {
        collatz(n / 2, count + 1)
      } else {
        collatz(3 * n + 1, count + 1)
      }
    }

    val rez = (1 until 1000000).map(x => (x, collatz(x, 1))).maxBy(_._2)._1
    assert(rez == 837799)
  }

  //Starting in the top left corner of a 2×2 grid, and only being able to move to the right and down, there are exactly
  //6 routes to the bottom right corner.How many such routes are there through a 20×20 grid?
  def p015(): Unit = {
    def rScan(row: List[Long], n: Int): List[Long] = {
      if (n == 0) {
        row
      }
      else {
        rScan(row.scan(0L)(_ + _).tail, n - 1)
      }
    }

    def nPaths(n: Int): Long = {
      val row = Seq.fill(n + 1)(1L).toList
      rScan(row, n).last
    }

    assert(nPaths(20) == 137846528820L)
  }

  //What is the sum of the digits of the number 2^1000?
  def p016(): Unit = {
    val rez = Seq.fill(1000)(BigInt(2)).product.toString.toCharArray.map(_.toString.toInt).sum
    assert(rez == 1366)
  }

  def p017(): Unit = {
    val words = Map(0 -> "", 1 -> "one", 2 -> "two", 3 -> "three", 4 -> "four", 5 -> "five", 6 -> "six", 7 -> "seven", 8 -> "eight",
      9 -> "nine", 10 -> "ten", 11 -> "eleven", 12 -> "twelve", 13 -> "thirteen", 14 -> "fourteen", 15 -> "fifteen", 16 -> "sixteen",
      17 -> "seventeen", 18 -> "eighteen", 19 -> "nineteen", 20 -> "twenty", 30 -> "thirty", 40 -> "forty", 50 -> "fifty", 60 -> "sixty", 70 -> "seventy",
      80 -> "eighty", 90 -> "ninety")

    def english(n: Int): String = {
      if (n < 20) { words(n) }
      else if (n < 100) {
        val tens = n / 10
        words(tens * 10) + english(n - tens*10)
      } else {
        val hundreds = n / 100
        val spacer = if (n % 100 == 0) "" else "and"
        words(hundreds) + "hundred" + spacer + english(n - 100 * hundreds)
      }
    }

    val rez = (0 to 999).map(english).mkString("").length + "onethousand".length
    assert(rez == 21124)
  }

}
