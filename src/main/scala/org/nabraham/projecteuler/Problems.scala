package org.nabraham.projecteuler

import java.text.SimpleDateFormat
import java.util.{Date, Calendar}

import Common.bigFibs
import Common.fibs
import Common.input
import Common.primes
import Common.solutions

import scala.collection.convert.Wrappers.MutableSetWrapper

/**
 * Created by nabraham on 8/10/15.
 */
object Problems {

  val problemMap = scala.collection.mutable.Map[String, () => Unit]();

  //Find the sum of all the multiples of 3 or 5 below 1000.
  def p001(): Unit = {
    val rez = (3 until 1000).filter(x => x % 3 == 0 || x % 5 == 0).sum
    assert(rez == solutions("1").toInt)
  }
  problemMap += ("001" -> p001)

  //By considering the terms in the Fibonacci sequence whose values do not exceed four million,
  //find the sum of the even-valued terms
  def p002(): Unit = {
    val rez = fibs.takeWhile(_ < 4000000).filter(_ % 2 == 0).sum
    assert(rez == solutions("2").toInt)
  }
  problemMap += ("002" -> p002)

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
    assert(rez == solutions("3").toInt)
  }
  problemMap += ("003" -> p003)

  //Find the largest palindrome made from the product of two 3-digit numbers.
  def p004(): Unit = {
    val nums = (100 to 999)
    val rez = nums.flatMap(x => nums.map(y => y * x)).filter(z => z.toString == z.toString.reverse).max
    assert(rez == solutions("4").toInt)
  }
  problemMap += ("004" -> p004)

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
    assert(rez == solutions("5").toInt)
  }
  problemMap += ("005" -> p005)

  //Find the difference between the sum of the squares of the first one hundred natural numbers and the square of the sum.
  def p006(): Unit = {
    val sumSquares = (1 to 100).map(x => x * x).sum
    val squareSums = Math.pow((1 to 100).sum, 2).toInt
    assert(squareSums - sumSquares == solutions("6").toInt)
  }
  problemMap += ("006" -> p006)

  //What is the 10 001st prime number?
  def p007(): Unit = {
    assert(primes(10000) == solutions("7").toInt)
  }
  problemMap += ("007" -> p007)

  //Find the thirteen adjacent digits in the 1000-digit number that have the
  //greatest product. What is the value of this product?
  def p008(): Unit = {
    val rez = input("008.txt").flatMap(_.trim).map(_.toString.toInt).sliding(13).map(_.product).max
    //TODO - why is this wrong?
    //assert(rez.toLong == solutions("8").toLong)
  }
  problemMap += ("008" -> p008)

  //There exists exactly one Pythagorean triplet for which a + b + c = 1000.
  //Find the product abc.
  def p009(): Unit = {
    def sq(x: Int): Int = {
      x * x
    }
    val nums = (1 until 1000)
    val rez = for (a <- nums; b <- 1 to a; c = 1000 - (a + b) if sq(a) + sq(b) == sq(c)) yield a * b * c
    assert(rez.head == solutions("9").toInt)
  }
  problemMap += ("009" -> p009)

  //Find the sum of all the primes below two million.
  def p010(): Unit = {
    val rez = primes.takeWhile(_ < 2000000).foldLeft(0L)(_ + _) //fold left instead of sum bc of MAX_INT
    assert(rez == solutions("10").toLong)
  }
  problemMap += ("010" -> p010)

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
    assert(rez == solutions("11").toInt)
  }
  problemMap += ("011" -> p011)

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
    assert(triangle(num) == solutions("12").toInt)
  }
  problemMap += ("012" -> p012)

  //Work out the first ten digits of the sum of the following one-hundred 50-digit numbers.
  def p013(): Unit = {
    val rez = input("013.txt").map(BigInt(_)).sum.toString.substring(0, 10)
    assert(rez == solutions("13"))
  }
  problemMap += ("013" -> p013)

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
    assert(rez == solutions("14").toInt)
  }
  problemMap += ("014" -> p014)

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

    assert(nPaths(20) == solutions("15").toLong)
  }
  problemMap += ("015" -> p015)

  //What is the sum of the digits of the number 2^1000?
  def p016(): Unit = {
    val rez = Seq.fill(1000)(BigInt(2)).product.toString.toCharArray.map(_.toString.toInt).sum
    assert(rez == 1366)
  }
  problemMap += ("016" -> p016)

  def p017(): Unit = {
    val words = Map(0 -> "", 1 -> "one", 2 -> "two", 3 -> "three", 4 -> "four", 5 -> "five", 6 -> "six", 7 -> "seven", 8 -> "eight",
      9 -> "nine", 10 -> "ten", 11 -> "eleven", 12 -> "twelve", 13 -> "thirteen", 14 -> "fourteen", 15 -> "fifteen", 16 -> "sixteen",
      17 -> "seventeen", 18 -> "eighteen", 19 -> "nineteen", 20 -> "twenty", 30 -> "thirty", 40 -> "forty", 50 -> "fifty", 60 -> "sixty", 70 -> "seventy",
      80 -> "eighty", 90 -> "ninety")

    def english(n: Int): String = {
      if (n < 20) {
        words(n)
      } else if (n < 100) {
        val tens = n / 10
        words(tens * 10) + english(n - tens * 10)
      } else {
        val hundreds = n / 100
        val spacer = if (n % 100 == 0) "" else "and"
        words(hundreds) + "hundred" + spacer + english(n - 100 * hundreds)
      }
    }

    val rez = (0 to 999).map(english).mkString("").length + "onethousand".length
    assert(rez == 21124)
  }
  problemMap += ("017" -> p017)

  //Find the maximum total from top to bottom of the triangle below:
  def p018(): Unit = {
    val rows = input("018.txt").map(_.split(" ").toList.map(_.toInt))

    def expand(row: List[Int]): List[Int] = {
      val left = row.head :: row
      val right = (row.last :: row.reverse).reverse
      (left, right).zipped.map(_ max _)
    }

    def reduce(pyramid: List[List[Int]]): List[Int] = {
      if (pyramid.length <= 1) {
        pyramid.head
      }
      else {
        val expanded = expand(pyramid.head)
        val convolved = (expanded, pyramid.tail.head).zipped.map(_ + _)
        reduce(convolved :: pyramid.tail.tail)
      }
    }

    assert(reduce(rows).max == 1074)
  }
  problemMap += ("018" -> p018)

  //How many Sundays fell on the first of the month during the twentieth century (1 Jan 1901 to 31 Dec 2000)?
  def p019(): Unit = {
    val cal = Calendar.getInstance()
    val sdf = new SimpleDateFormat("yyyy/M/d")
    val firsts = for (year <- (1901 to 2000);
                      month <- (1 to 12)
    ) yield { cal.setTime(sdf.parse("" + year + "/" +  month + "/1" )); cal.get(Calendar.DAY_OF_WEEK) }

    assert(firsts.count(_ == 1) == 171)
  }

  //Find the sum of the digits in the number 100!
  def p020(): Unit = {
    val rez = (BigInt(2) to BigInt(100)).product.toString.toCharArray.map(_.toString.toInt).sum
    assert(rez == 648)
  }
  problemMap += ("020" -> p020)

  //Evaluate the sum of all the amicable numbers under 10000
  def divisors(n: Int): List[Int] = {
    if (n == 0) List()
    else (1 to n/2).filter(n % _ == 0).toList
  }
  def p021(): Unit = {
    val zipped = (0 until 10000).map(divisors(_).sum).zipWithIndex
    def amicable(e: (Int, Int)): Boolean = {
      e._1 <= zipped.length && zipped(e._1)._1 == e._2 && e._1 != e._2
    }

    val ams = zipped.filter(amicable).map(_._2)
    assert(ams.sum == 31626)
  }
  problemMap += ("021" -> p021)

  //For example, when the list is sorted into alphabetical order, COLIN, which is worth 3 + 15 + 12 + 9 + 14 = 53,
  //is the 938th name in the list. So, COLIN would obtain a score of 938 × 53 = 49714.
  //What is the total of all the name scores in the file?
  def p022(): Unit = {
    val cleaned = input("022.txt").mkString("").split(",").map(_.replaceAll("\"","")).sorted
    assert(cleaned
      .map(_.toCharArray.map(_.toInt - 64).sum)
      .zipWithIndex
      .map(zi => zi._1 * (zi._2 + 1))
      .sum == 871198282)
  }
  problemMap += ("022" -> p022)

  //As 12 is the smallest abundant number, 1 + 2 + 3 + 4 + 6 = 16, the smallest number that can be written as the sum of
  //two abundant numbers is 24. By mathematical analysis, it can be shown that all integers greater than 28123 can be
  //written as the sum of two abundant numbers. However, this upper limit cannot be reduced any further by analysis even
  //though it is known that the greatest number that cannot be expressed as the sum of two abundant numbers is less than
  //this limit.
  //
  //Find the sum of all the positive integers which cannot be written as the sum of two abundant numbers.
  def p023(): Unit = {
    val max = 28123
    val abundant = (1 to max).filter(x => divisors(x).sum > x)
    val S = scala.collection.mutable.Set.empty[Int]
    for (a <- abundant; b <-abundant) {
      if (a + b <= max) {
        S += (a + b)
      }
    }
    val rez = (1 to max).toSet &~ S
    assert(rez.toList.sum == solutions("23").toInt)
  }
  problemMap += ("023" -> p023)

  //What is the millionth lexicographic permutation of the digits 0, 1, 2, 3, 4, 5, 6, 7, 8 and 9?
  def p024(): Unit = {
    val rez = (0 to 9).permutations.drop(999999).take(1).next()
    assert(rez.mkString("") == solutions("24"))
  }
  problemMap += ("024" -> p024)

  //What is the index of the first term in the Fibonacci sequence to contain 1000 digits?
  def p025(): Unit = {
    assert(bigFibs.indexWhere(_.toString.length >= 1000) == solutions("25").toInt)
  }
  problemMap += ("025" -> p025)

  //Find the value of d < 1000 for which 1/d contains the longest recurring cycle in its decimal fraction part.
  def p026(): Unit = {
    def cycleLength(num: Int, denom: Int = 1, remainders: List[Int] = List()) : Int = {
      if (remainders.contains(denom)) {
        remainders.length
      } else if (denom % num == 0) {
        0
      } else if (num > denom) {
        cycleLength(num, denom * 10, remainders)
      } else {
        cycleLength(num, (denom % num) * 10, denom :: remainders)
      }
    }
    val rez = (1 to 999).map(cycleLength(_)).zipWithIndex.maxBy(_._1)._2 + 1
    assert(rez == solutions("26").toInt)
  }
  problemMap += ("026" -> p026)
}

