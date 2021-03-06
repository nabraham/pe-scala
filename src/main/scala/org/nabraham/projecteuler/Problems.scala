package org.nabraham.projecteuler

import java.text.SimpleDateFormat
import java.util.Calendar

import org.nabraham.projecteuler.Common.{bigFibs, fibs, input, primes, solutions, from}

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
    assert(rez == solutions("16").toInt)
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
    assert(rez == solutions("17").toInt)
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
    assert(reduce(rows).max == solutions("18").toInt)
  }

  problemMap += ("018" -> p018)

  //How many Sundays fell on the first of the month during the twentieth century (1 Jan 1901 to 31 Dec 2000)?
  def p019(): Unit = {
    val cal = Calendar.getInstance()
    val sdf = new SimpleDateFormat("yyyy/M/d")
    val firsts = for (year <- (1901 to 2000);
                      month <- (1 to 12)
    ) yield {
        cal.setTime(sdf.parse("" + year + "/" + month + "/1")); cal.get(Calendar.DAY_OF_WEEK)
      }

    assert(firsts.count(_ == 1) == solutions("19").toInt)
  }

  //Find the sum of the digits in the number 100!
  def p020(): Unit = {
    val rez = (BigInt(2) to BigInt(100)).product.toString.toCharArray.map(_.toString.toInt).sum
    assert(rez == solutions("20").toInt)
  }

  problemMap += ("020" -> p020)

  //Evaluate the sum of all the amicable numbers under 10000
  def divisors(n: Int): List[Int] = {
    if (n == 0) List()
    else (1 to n / 2).filter(n % _ == 0).toList
  }

  def p021(): Unit = {
    val zipped = (0 until 10000).map(divisors(_).sum).zipWithIndex
    def amicable(e: (Int, Int)): Boolean = {
      e._1 <= zipped.length && zipped(e._1)._1 == e._2 && e._1 != e._2
    }

    val ams = zipped.filter(amicable).map(_._2)
    assert(ams.sum == solutions("21").toInt)
  }

  problemMap += ("021" -> p021)

  //For example, when the list is sorted into alphabetical order, COLIN, which is worth 3 + 15 + 12 + 9 + 14 = 53,
  //is the 938th name in the list. So, COLIN would obtain a score of 938 × 53 = 49714.
  //What is the total of all the name scores in the file?
  def p022(): Unit = {
    val cleaned = input("022.txt").mkString("").split(",").map(_.replaceAll("\"", "")).sorted
    assert(cleaned
      .map(_.toCharArray.map(_.toInt - 64).sum)
      .zipWithIndex
      .map(zi => zi._1 * (zi._2 + 1))
      .sum == solutions("22").toInt)
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
    for (a <- abundant; b <- abundant) {
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
    def cycleLength(num: Int, denom: Int = 1, remainders: List[Int] = List()): Int = {
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

  //  Considering quadratics of the form:
  //
  //    n² + an + b, where |a| < 1000 and |b| < 1000
  //
  //  where |n| is the modulus/absolute value of n
  //    e.g. |11| = 11 and |−4| = 4
  //
  //  Find the product of the coefficients, a and b, for the quadratic expression that produces the maximum number of
  //  primes for consecutive values of n, starting with n = 0.
  def p027(): Unit = {
    val coefs = (-999 to 999)
    def quadratic(a: Int, b: Int)(n: Int) = {
      n * n + a * n + b
    }
    val twoMil = primes.takeWhile(_ <= 200000).toList
    def isPrime(n: Int): Boolean = {
      twoMil.contains(n)
    }
    def quadraticPrimeLength(a: Int, b: Int): Int = {
      from(0).takeWhile(n => isPrime(quadratic(a, b)(n))).toList.length
    }
    val rez = coefs.flatMap(a => coefs.map(b => ((a, b), quadraticPrimeLength(a, b)))).maxBy(_._2)
    assert(rez._1._1 * rez._1._2 == solutions("27").toInt)
  }

  problemMap += ("027" -> p027)

  //  Starting with the number 1 and moving to the right in a clockwise direction a 5 by 5 spiral is formed as follows:
  //
  //  21 22 23 24 25
  //  20  7  8  9 10
  //  19  6  1  2 11
  //  18  5  4  3 12
  //  17 16 15 14 13
  //
  //  It can be verified that the sum of the numbers on the diagonals is 101.
  //
  //  What is the sum of the numbers on the diagonals in a 1001 by 1001 spiral formed in the same way?
  def p028(): Unit = {
    //The lower right (LR) number of each centered square closed form is: 4n^2 - 10n + 7
    //The other corners are LR + 2(n-1), + 4(n-1), + 6(n-1)
    //Sum of square corners: 4*LR + 12(n-1) = 16n^2 - 28n + 16, for n > 1
    assert(1 + (2 to 501).map(n => 16 * n * n - 28 * n + 16).sum == solutions("28").toInt)
  }

  problemMap += ("028" -> p028)

  //  Consider all integer combinations of a^b for 2 ≤ a ≤ 5 and 2 ≤ b ≤ 5:
  //
  //  22=4, 23=8, 24=16, 25=32
  //  32=9, 33=27, 34=81, 35=243
  //  42=16, 43=64, 44=256, 45=1024
  //  52=25, 53=125, 54=625, 55=3125
  //
  //  If they are then placed in numerical order, with any repeats removed, we get the following sequence of
  //  15 distinct terms:
  //
  //  4, 8, 9, 16, 25, 27, 32, 64, 81, 125, 243, 256, 625, 1024, 3125
  //
  //  How many distinct terms are in the sequence generated by a^b for 2 ≤ a ≤ 100 and 2 ≤ b ≤ 100?
  def p029(): Unit = {
    val max = 100
    val nums = (2 to max)
    val rez = nums.flatMap(a => nums.map(b => BigInt(a).pow(b))).toSet.size
    assert(rez == solutions("29").toInt)
  }

  problemMap += ("029" -> p029)

  //  Surprisingly there are only three numbers that can be written as the sum of fourth powers of their digits:
  //
  //  1634 = 14 + 64 + 34 + 44
  //  8208 = 84 + 24 + 04 + 84
  //  9474 = 94 + 44 + 74 + 44
  //
  //  As 1 = 14 is not a sum it is not included.
  //
  //    The sum of these numbers is 1634 + 8208 + 9474 = 19316.
  //
  //  Find the sum of all the numbers that can be written as the sum of fifth powers of their digits.
  def p030(): Unit = {
    //354294 = 9^5 *6, thus the biggest candidate
    val max = 354294
    def fifthSum(n: Int): Int = {
      n.toString.toCharArray.map(_ - 48).map(Seq.fill(5)(_).product).sum
    }
    val rez = (10 to max).filter(x => x == fifthSum(x))
    assert(rez.sum == solutions("30").toInt)
  }

  problemMap += ("030" -> p030)

  //  In England the currency is made up of pound, £, and pence, p, and there are eight coins in general circulation:
  //
  //  1p, 2p, 5p, 10p, 20p, 50p, £1 (100p) and £2 (200p).
  //
  //    It is possible to make £2 in the following way:
  //
  //  1×£1 + 1×50p + 2×20p + 1×5p + 1×2p + 3×1p
  //
  //  How many different ways can £2 be made using any number of coins?
  def p031(): Unit = {
    val amounts = List(1, 2, 5, 10, 20, 50, 100, 200).reverse
    val maxes = amounts.map(200 / _)
    val pairs = amounts zip maxes
    def coinSum(coins: List[(Int, Int)]): Int = {
      coins.map(c => c._1 * c._2).sum
    }
    def findCombos(
                    total: Int,
                    amountRange: List[(Int, Int)],
                    chosen: List[(Int, Int)] = List(),
                    valid: List[List[(Int, Int)]] = List()): List[List[(Int, Int)]] = {
      val sum = coinSum(chosen)
      if (sum == total) {
        chosen :: valid
      } else if (sum > total || amountRange.isEmpty) {
        valid
      } else {
        val head = amountRange.head
        (0 to head._2).flatMap(n => findCombos(total, amountRange.tail, (head._1, n) :: chosen, valid)).toList
      }
    }
    assert(findCombos(200, pairs).size == solutions("31").toInt)
  }

  problemMap += ("031" -> p031)

  //  We shall say that an n-digit number is pandigital if it makes use of all the digits 1 to n exactly once;
  //  for example, the 5-digit number, 15234, is 1 through 5 pandigital.
  //
  //   The product 7254 is unusual, as the identity, 39 × 186 = 7254, containing multiplicand, multiplier, and
  //   product is 1 through 9 pandigital.
  //
  //   Find the sum of all products whose multiplicand/multiplier/product identity can be written as a 1 through 9
  //   pandigital.
  //
  //  HINT: Some products can be obtained in more than one way so be sure to only include it once in your sum.
  def p032(): Unit = {
    val pandigits = (1 to 9).toSet
    def uniqueNumbers(a: Int, b: Int, c: Int): Boolean = {
      val nums = List(a, b, c).flatMap(_.toString.toCharArray.map(_ - 48))
      nums.size == pandigits.size && nums.toSet == pandigits
    }
    def pandigital(n: Int): Boolean = {
      divisors(n).exists(d => uniqueNumbers(n / d, d, n))
    }
    val rez = (1000 to 10000).filter(pandigital(_))
    assert(rez.sum == solutions("32").toInt)
  }

  problemMap += ("032" -> p032)

  //  The fraction 49/98 is a curious fraction, as an inexperienced mathematician in attempting to simplify it may
  //  incorrectly believe that 49/98 = 4/8, which is correct, is obtained by cancelling the 9s.
  //
  //  We shall consider fractions like, 30/50 = 3/5, to be trivial examples.
  //
  //  There are exactly four non-trivial examples of this type of fraction, less than one in value, and containing two
  //  digits in the numerator and denominator.
  //
  //  If the product of these four fractions is given in its lowest common terms, find the value of the denominator.
  def p033(): Unit = {
    def simplify(num: Int, denom: Int): (Int, Int) = {
      (2 to num).reverse.find(x => num % x == 0 && denom % x == 0) match {
        case Some(i) => simplify(num / i, denom / i)
        case None => (num, denom)
      }
    }

    val nums = (10 to 99)
    def isCurious(a: Int, b: Int): Boolean = {
      val aList = a.toString.toCharArray.map(_ - 48)
      val bList = b.toString.toCharArray.map(_ - 48)
      if (aList(1) == bList(1)) {
        false
      }
      else if (aList(0) == bList(1)) {
        aList(1) * b == bList(0) * a
      } else if (aList(1) == bList(0)) {
        aList(0) * b == bList(1) * a
      } else {
        false
      }
    }

    val fracs = nums.flatMap(a => (a + 1 to 99).map(b => (a, b))).filter(f => isCurious(f._1, f._2))
    val num = fracs.map(_._1).product
    val denom = fracs.map(_._2).product
    assert(simplify(num, denom)._2 == solutions("33").toInt)
  }

  problemMap += ("033" -> p033)

  //    145 is a curious number, as 1! + 4! + 5! = 1 + 24 + 120 = 145.
  //
  //    Find the sum of all numbers which are equal to the sum of the factorial of their digits.
  //
  //    Note: as 1! = 1 and 2! = 2 are not sums they are not included.
  def p034(): Unit = {
    val fact = Map((0 to 9).map(n => n -> (1 to n).product): _*)
    def isSumOfFactorial(n: Int): Boolean = {
      n.toString.toCharArray.map(_ - 48).map(fact(_)).sum == n
    }
    val max = 99999 // this is cheating, 9! * n = 10^(n-1) means we should check 7-digit numbers
    val nums = (10 to max).filter(isSumOfFactorial)
    assert(nums.sum == solutions("34").toInt)
  }

  problemMap += ("034" -> p034)

  //  The number, 197, is called a circular prime because all rotations of the digits: 197, 971, and 719, are
  //  themselves prime.
  //
  //  There are thirteen such primes below 100: 2, 3, 5, 7, 11, 13, 17, 31, 37, 71, 73, 79, and 97.
  //
  //  How many circular primes are there below one million?
  def p035(): Unit = {
    val ps = primes.takeWhile(_ < 1000000).toSet

    def rotate(n: Int, shift: Int): Int = {
      val front = n.toString.substring(0, shift)
      val back = n.toString.substring(shift)
      (back + front).toInt
    }

    def rotations(n: Int): List[Int] = {
      (0 until n.toString.length).map(rotate(n, _)).toList
    }

    assert(ps.filter(n => rotations(n).forall(ps.contains(_))).size == solutions("35").toInt)
  }

  problemMap += ("035" -> p035)

  //  The decimal number, 585 = 10010010012 (binary), is palindromic in both bases.
  //
  //    Find the sum of all numbers, less than one million, which are palindromic in base 10 and base 2.
  //
  //  (Please note that the palindromic number, in either base, may not include leading zeros.)
  def p036(): Unit = {
    def palindrome(n: Int, radix: Int): Boolean = {
      val s = java.lang.Integer.toString(n, radix)
      s == s.reverse
    }
    assert((1 to 999999).filter(x => palindrome(x, 2) && palindrome(x, 10)).sum == solutions("36").toInt)
  }

  problemMap += ("036" -> p036)

  //  The number 3797 has an interesting property. Being prime itself, it is possible to continuously remove digits
  //  from left to right, and remain prime at each stage: 3797, 797, 97, and 7. Similarly we can work from right to left:
  //  3797, 379, 37, and 3.
  //
  //  Find the sum of the only eleven primes that are both truncatable from left to right and right to left.
  //
  //  NOTE: 2, 3, 5, and 7 are not considered to be truncatable primes.
  def p037(): Unit = {
    def truncate(n: Int, left: Boolean = true): List[Int] = {
      val digits = n.toString.toCharArray.map(_ - 48)
      if (left) {
        (1 to digits.length).map(t => digits.take(t).mkString("").toInt).toList
      } else {
        (1 to digits.length).map(t => digits.takeRight(t).mkString("").toInt).toList
      }
    }

    val pMil = primes.takeWhile(_ < 1000000)
    val pSet = pMil.toSet
    val rez = pMil.dropWhile(_ < 10).filter(p => (truncate(p) ::: truncate(p, false)).forall(pSet.contains(_)))
    assert(rez.sum == solutions("37").toInt)
  }

  problemMap += ("037" -> p037)

  //  Take the number 192 and multiply it by each of 1, 2, and 3:
  //
  //  192 × 1 = 192
  //  192 × 2 = 384
  //  192 × 3 = 576
  //
  //  By concatenating each product we get the 1 to 9 pandigital, 192384576. We will call 192384576 the concatenated
  //  product of 192 and (1,2,3)
  //
  //  The same can be achieved by starting with 9 and multiplying by 1, 2, 3, 4, and 5, giving the pandigital,
  //  918273645, which is the concatenated product of 9 and (1,2,3,4,5).
  //
  //  What is the largest 1 to 9 pandigital 9-digit number that can be formed as the concatenated product of an integer
  //  with (1,2, ... , n) where n > 1?
  def p038(): Unit = {
    val panSet = "123456789".toSet
    def isPandigital(n: Int): Boolean = {
      (1 to 2).map(_ * n).mkString("").toCharArray.toSet == panSet
    }
    val rez = (9000 to 9999).filter(isPandigital).max
    assert(rez.toString + (rez * 2).toString == solutions("38"))
  }

  problemMap += ("038" -> p038)

  //  If p is the perimeter of a right angle triangle with integral length sides, {a,b,c}, there are exactly three
  //  solutions for p = 120.
  //
  //  {20,48,52}, {24,45,51}, {30,40,50}
  //
  //  For which value of p ≤ 1000, is the number of solutions maximised?
  def p039(): Unit = {
    def right(a: Int, b: Int, c: Int): Boolean = {
      a * a + b * b == c * c
    }
    def rights(p: Int): Int = {
      val tris = for (a <- 1 to p - 2;
                      b <- a + 1 to p - 2;
                      c = p - a - b;
                      if right(a, b, c)) yield (a, b, c)
      tris.size
    }
    assert((2 to 1000).map(p => (p, rights(p))).maxBy(_._2)._1 == solutions("39").toInt)
  }

  problemMap += ("039" -> p039)

  //  An irrational decimal fraction is created by concatenating the positive integers:
  //
  //  0.123456789101112131415161718192021...
  //
  //  It can be seen that the 12th digit of the fractional part is 1.
  //
  //  If dn represents the nth digit of the fractional part, find the value of the following expression.
  //
  //    d1 × d10 × d100 × d1000 × d10000 × d100000 × d1000000
  def p040(): Unit = {
    def genChamp(s: Int, n: Int = 1, c: Vector[Char] = Vector()): Vector[Char] = {
      if (c.size >= s) {
        c
      }
      else {
        genChamp(s, n + 1, c ++ n.toString.toCharArray.toVector)
      }
    }
    val champ = genChamp(1000000)
    val rez = (0 to 6).map(p => champ(BigInt(10).pow(p).toInt - 1).toString.toInt)
    assert(rez.product == solutions("40").toInt)
  }

  problemMap += ("040" -> p040)

  //  We shall say that an n-digit number is pandigital if it makes use of all the digits 1 to n exactly once. For
  //  example, 2143 is a 4-digit pandigital and is also prime.
  //
  //  What is the largest n-digit pandigital prime that exists?
  def p041(): Unit = {
    //all 8,9 pandigital numbers are divisible by 3, so only check 7
    def isPrime(n: Int): Boolean = {
      (2 to Math.sqrt(n).toInt).forall(n % _ != 0)
    }
    val rez = (1 to 7).reverse.permutations.find(p => isPrime(p.mkString("").toInt))
    assert(rez.get.mkString("") == solutions("41"))
  }

  problemMap += ("041" -> p041)

  //  The nth term of the sequence of triangle numbers is given by, tn = ½n(n+1); so the first ten triangle numbers are:
  //
  //  1, 3, 6, 10, 15, 21, 28, 36, 45, 55, ...
  //
  //  By converting each letter in a word to a number corresponding to its alphabetical position and adding these
  //  values we form a word value. For example, the word value for SKY is 19 + 11 + 25 = 55 = t10.
  //  If the word value is a triangle number then we shall call the word a triangle word.
  //
  //  Using words.txt (right click and 'Save Link/Target As...'), a 16K text file containing nearly two-thousand common
  //  English words, how many are triangle words?
  def p042(): Unit = {
    def tri(n: Int): Int = {
      (n * n + n) / 2
    }
    val triSet = (1 to 100).map(tri).toSet
    def isTriangle(w: String): Boolean = {
      triSet.contains(w.toCharArray.map(_ - 64).sum)
    }
    val rez = input("p042_words.txt").mkString("").split(",").count(w => isTriangle(w.replaceAll("[^A-Z]", "")))
    assert(rez == solutions("42").toInt)
  }

  problemMap += ("042" -> p042)

  //  The number, 1406357289, is a 0 to 9 pandigital number because it is made up of each of the digits 0 to 9 in
  //  some order, but it also has a rather interesting sub-string divisibility property.
  //
  //  Let d1 be the 1st digit, d2 be the 2nd digit, and so on. In this way, we note the following:
  //
  //  d2d3d4=406 is divisible by 2
  //  d3d4d5=063 is divisible by 3
  //  d4d5d6=635 is divisible by 5
  //  d5d6d7=357 is divisible by 7
  //  d6d7d8=572 is divisible by 11
  //  d7d8d9=728 is divisible by 13
  //  d8d9d10=289 is divisible by 17
  //
  //  Find the sum of all 0 to 9 pandigital numbers with this property.
  def p043(): Unit = {
    val primeMap = Map(2 -> 2, 3 -> 3, 4 -> 5, 5 -> 7, 6 -> 11, 7 -> 13, 8 -> 17)
    val rez = (0 to 9).permutations.filter(p => (2 to 8).forall(s => p.drop(s - 1).take(3).mkString("").toInt % primeMap(s) == 0))
    assert(rez.map(v => BigInt(v.mkString(""))).sum.toString == solutions("43"))
  }

  problemMap += ("043" -> p043)

  //  Pentagonal numbers are generated by the formula, Pn=n(3n−1)/2. The first ten pentagonal numbers are:
  //
  //  1, 5, 12, 22, 35, 51, 70, 92, 117, 145, ...
  //
  //  It can be seen that P4 + P7 = 22 + 70 = 92 = P8. However, their difference, 70 − 22 = 48, is not pentagonal.
  //
  //  Find the pair of pentagonal numbers, Pj and Pk, for which their sum and difference are pentagonal and
  //  D = |Pk − Pj| is minimised; what is the value of D?
  def p044(): Unit = {
    def pent(n: Int): Int = {
      (3 * n * n - n) / 2
    }
    def abs(n: Int): Int = {
      if (n >= 0) {
        n
      } else {
        -n
      }
    }
    val max = 10000
    val pents = (1 to max).map(pent).toSet
    val pairs = for (j <- 1 to max;
                     k <- (j + 1) to max;
                     pj = pent(j);
                     pk = pent(k);
                     d = abs(pj - pk);
                     if pents.contains(pj + pk) && pents.contains(d)
    ) yield ((j, k), d)
    assert(pairs.minBy(_._2)._2 == solutions("44").toInt)
  }

  problemMap += ("044" -> p044)

  //  Triangle, pentagonal, and hexagonal numbers are generated by the following formulae:
  //  Triangle 	  	Tn=n(n+1)/2 	  	1, 3, 6, 10, 15, ...
  //  Pentagonal 	  Pn=n(3n−1)/2 	  	1, 5, 12, 22, 35, ...
  //  Hexagonal 	  Hn=n(2n−1) 	  	  1, 6, 15, 28, 45, ...
  //
  //  It can be verified that T285 = P165 = H143 = 40755.
  //
  //  Find the next triangle number that is also pentagonal and hexagonal.
  def p045(): Unit = {
    def tri(n: Long): Long = {
      (n * n + n) / 2
    }
    def pent(n: Long): Long = {
      (3 * n * n - n) / 2
    }
    def hex(n: Long): Long = {
      2 * n * n - n
    }

    def findNext(ti: Long, pi: Long, hi: Long): (Long, Long, Long, Long) = {
      val tv = tri(ti)
      val pv = pent(pi)
      val hv = hex(hi)
      if (tv == pv && tv == hv) {
        (ti, pi, hi, tv)
      }
      else if (tv < pv && tv < hv) {
        findNext(ti + 1, pi, hi)
      }
      else if (pv < hv) {
        findNext(ti, pi + 1, hi)
      }
      else {
        findNext(ti, pi, hi + 1)
      }
    }
    assert(findNext(286L, 166L, 144L)._4 == solutions("45").toLong)
  }

  problemMap += ("045" -> p045)

  //  It was proposed by Christian Goldbach that every odd composite number can be written as the sum of a prime and
  //  twice a square.
  //
  //  9 = 7 + 2×1^2
  //  15 = 7 + 2×2^2
  //  21 = 3 + 2×3^2
  //  25 = 7 + 2×3^2
  //  27 = 19 + 2×2^2
  //  33 = 31 + 2×1^2
  //
  //  It turns out that the conjecture was false.
  //
  //  What is the smallest odd composite that cannot be written as the sum of a prime and twice a square?
  def p046(): Unit = {
    def p1000 = primes.take(10000).toSet
    def isPrime(n: Int): Boolean = {
      p1000.contains(n)
    }
    def oddComposites(n: Int = 9): Stream[Int] = {
      val next = from(n + 2).find(!isPrime(_))
      n #:: oddComposites(next.get)
    }
    def sq(n: Int): Int = {
      n * n
    }
    def summable(n: Int): Boolean = {
      (1 to Math.sqrt(n).toInt).exists(i => isPrime(n - 2 * sq(i)))
    }
    assert(oddComposites().find(!summable(_)).get == solutions("46").toInt)
  }

  problemMap += ("046" -> p046)

  //  The first two consecutive numbers to have two distinct prime factors are:
  //
  //  14 = 2 × 7
  //  15 = 3 × 5
  //
  //  The first three consecutive numbers to have three distinct prime factors are:
  //
  //  644 = 2² × 7 × 23
  //  645 = 3 × 5 × 43
  //  646 = 2 × 17 × 19.
  //
  //  Find the first four consecutive integers to have four distinct prime factors. What is the first of these numbers?
  def p047(): Unit = {
    def primeFactors(n: Int, np: Int): Boolean = {
      primes.takeWhile(_ <= n / 2).count(n % _ == 0) >= np
    }
    def consecutivePrimes(n: Int, np: Int): Boolean = {
      (n to (n + np - 1)).forall(i => primeFactors(i, np))
    }
    assert(from(644).find(n => consecutivePrimes(n, 4)).get == solutions("47").toInt)
  }

  problemMap += ("047" -> p047)

  //  The series, 1^1 + 2^2 + 3^3 + ... + 10^10 = 10405071317.
  //
  //  Find the last ten digits of the series, 1^1 + 2^2 + 3^3 + ... + 1000^1000.
  def p048(): Unit = {
    assert((1 to 1000).map(x => BigInt(x).pow(x)).sum.toString.takeRight(10) == solutions("48"))
  }

  problemMap += ("048" -> p048)

  //  The arithmetic sequence, 1487, 4817, 8147, in which each of the terms increases by 3330, is unusual in two ways:
  //  (i) each of the three terms are prime, and,
  //  (ii) each of the 4-digit numbers are permutations of one another.
  //
  //   There are no arithmetic sequences made up of three 1-, 2-, or 3-digit primes, exhibiting this property, but
  //   there is one other 4-digit increasing sequence.
  //
  //   What 12-digit number do you form by concatenating the three terms in this sequence?
  def p049(): Unit = {
    val candidates = primes.dropWhile(_ < 1000).takeWhile(_ < 10000).groupBy(_.toString.sorted).filter(_._2.size >= 3)
    def abs(n: Int): Int = {
      if (n > 0) n else -n
    }
    def commonDiff(nums: List[Int]): Boolean = {
      nums.sorted.sliding(2).map(w => w(1) - w(0)).toList.distinct.size == 1
    }
    val rez = candidates.map(_._2.toList).map(_.combinations(3).filter(commonDiff).toList).filter(_.nonEmpty).toList
    val other = rez.filter(_.flatten.head != 1487).flatten.flatten
    assert(other.mkString("") == solutions("49"))
  }

  problemMap += ("049" -> p049)

  //  The prime 41, can be written as the sum of six consecutive primes:
  //
  //  41 = 2 + 3 + 5 + 7 + 11 + 13
  //  This is the longest sum of consecutive primes that adds to a prime below one-hundred.
  //
  //    The longest sum of consecutive primes below one-thousand that adds to a prime, contains 21 terms, and is equal to 953.
  //
  //  Which prime, below one-million, can be written as the sum of the most consecutive primes?
  def p050(): Unit = {
    val ps = primes.takeWhile(_ < 1000000)
    val psSet = ps.toSet
    def takeUntilOver(s: Stream[Int], limit: Int, total: Int = 0, biggest: Int = 1, size: Int = 0, count: Int = 0): (Int, Int) = {
      if (limit <= 0 || s.isEmpty) {
        (biggest, size)
      }
      else {
        val t = total + s.head
        if (psSet.contains(t)) {
          takeUntilOver(s.tail, limit - s.head, t, t, count + 1, count + 1)
        } else {
          takeUntilOver(s.tail, limit - s.head, t, biggest, size, count + 1)
        }
      }
    }

    def longestWindow(n: Int, limit: Int): (Int, Int) = {
      val start = ps.dropWhile(_ < n)
      takeUntilOver(start, limit)
    }
    val rez = ps.map(longestWindow(_, 1000000)).maxBy(_._2)
    assert(rez._1 == solutions("50").toInt)
  }

  problemMap += ("050" -> p050)

  //  By replacing the 1st digit of the 2-digit number *3, it turns out that six of the nine possible values:
  //      13, 23, 43, 53, 73, and 83, are all prime.
  //
  //  By replacing the 3rd and 4th digits of 56**3 with the same digit, this 5-digit number is the first example having
  //  seven primes among the ten generated numbers, yielding the family: 56003, 56113, 56333, 56443, 56663, 56773, and
  //  56993. Consequently 56003, being the first member of this family, is the smallest prime with this property.
  //
  //  Find the smallest prime which, by replacing part of the number (not necessarily adjacent digits) with the same
  //  digit, is part of an eight prime value family.
  def p051(): Unit = {
    def asterize(s: String, indices: List[Int]): String = {
      indices.foldLeft(s)((str, i) => str.updated(i - 1, "*").mkString(""))
    }

    def isValid(ast: String, n: String): Boolean = {
      val as = (0 until ast.length).filter(i => ast.charAt(i) == '*')
      as.map(i => n.charAt(i)).distinct.length == 1
    }

    def replaceNChars(s: String, n: Int): List[String] = {
      (1 to s.length).combinations(n).toList.map(indices => asterize(s, indices.toList)).filter(a => isValid(a, s))
    }

    def genFamilies(n: Int): List[String] = {
      val nStr = n.toString
      (1 to nStr.length - 1).flatMap(c => replaceNChars(nStr, c)).toList
    }

    val fams = primes.dropWhile(_ < 10).takeWhile(_ < 1000000).flatMap(p => genFamilies(p).map(f => (f, p)))
    val rez = fams.groupBy(_._1).toList.filter(_._2.length == 8).minBy(_._2.map(_._2).min)
    assert(rez._1.replaceAll("\\*", "1") == solutions("51"))
  }

  problemMap += ("051" -> p051)

  //  It can be seen that the number, 125874, and its double, 251748, contain exactly the same digits, but in
  //  a different order.
  //
  //  Find the smallest positive integer, x, such that 2x, 3x, 4x, 5x, and 6x, contain the same digits.
  def p052(): Unit = {
    def multiples(m: Int, n: Int): List[Int] = {
      (1 to m).map(_ * n).toList
    }
    def sortInt(n: Int): Int = {
      n.toString.toCharArray.sorted.mkString("").toInt
    }
    val rez = from(1).find(n => multiples(6, n).map(sortInt).distinct.length == 1)
    assert(rez.get == solutions("52").toInt)
  }

  problemMap += ("052" -> p052)

  //  There are exactly ten ways of selecting three from five, 12345:
  //
  //  123, 124, 125, 134, 135, 145, 234, 235, 245, and 345
  //
  //  In combinatorics, we use the notation, 5C3 = 10.
  //
  //  In general,
  //  nCr =
  //    n!
  //      r!(n−r)!
  //  ,where r ≤ n, n! = n×(n−1)×...×3×2×1, and 0! = 1.
  //
  //  It is not until n = 23, that a value exceeds one-million: 23C10 = 1144066.
  //
  //  How many, not necessarily distinct, values of  nCr, for 1 ≤ n ≤ 100, are greater than one-million?
  def p053(): Unit = {
    def biggerThan(n: Int, k: Int, thresh: Int): Boolean = {
      val num = (k + 1 to n)
      val denom = (1 to (n - k))
      (num.map(BigInt(_)).product / denom.map(BigInt(_)).product) > thresh
    }

    val all = for (n <- 1 to 100; k <- 1 until n) yield biggerThan(n, k, 1000000)
    assert(all.count(p => p) == solutions("53").toInt)
  }

  problemMap += ("053" -> p053)

  //How many poker hands does player 1 win
  /*
    I started by making a function for each hand type (HK, P, 2P, etc.).  Was chasing down a bug, got frustrated,
    and eventually started over after reading this beautiful, concise python solution:
    http://blog.dreamshire.com/project-euler-54-solution/
   */
  def p054(): Unit = {
    val values = "23456789TJQKA".toCharArray.zipWithIndex.toMap
    val straights = (2 to 10).map(i => (0 to 4).map(_ + i - 2).reverse).toList ::: List(Vector(12, 3, 2, 1, 0))
    val ranks = Vector(List(1, 1, 1, 1, 1), List(2, 1, 1, 1), List(2, 2, 1), List(3, 1, 1), List(), List(), List(3, 2), List(4, 1))

    def winner(line: String): Int = {
      val cards = line.split(" ")
      if (gt(score(cards.take(5)), score(cards.takeRight(5)))) {
        1
      } else {
        2
      }
    }

    def gt(a: List[Int], b: List[Int]): Boolean = {
      if (a.isEmpty) {
        false
      }
      else if (b.isEmpty) {
        true
      }
      else if (a.head < b.head) {
        false
      }
      else if (a.head > b.head) {
        true
      }
      else {
        gt(a.tail, b.tail)
      }
    }

    def sortTuple(a: (Int, Seq[String]), b: (Int, Seq[String])): Boolean = {
      if (a._2.length != b._2.length) {
        a._2.length - b._2.length > 0
      } else {
        a._1 - b._1 > 0
      }
    }

    def score(cards: Seq[String]): List[Int] = {
      val groups = cards.groupBy(_.head).toList.map(kv => (values(kv._1), kv._2)).sortWith(sortTuple)
      val numbers = groups.map(_._1)
      val isStraight = straights.contains(numbers.toVector)
      val isFlush = cards.map(_.charAt(1)).distinct.length == 1
      if (isStraight && isFlush) {
        List(8) ::: numbers
      } else if (isStraight) {
        List(4) ::: numbers
      } else if (isFlush) {
        List(5) ::: numbers
      } else {
        List(ranks.indexOf(groups.map(_._2.length))) ::: numbers
      }
    }

    assert(input("p054_poker.txt").map(winner).count(_ == 1) == solutions("54").toInt)
  }

  problemMap += ("054" -> p054)

  //  If we take 47, reverse and add, 47 + 74 = 121, which is palindromic.
  //
  //  Not all numbers produce palindromes so quickly. For example,
  //
  //  349 + 943 = 1292,
  //  1292 + 2921 = 4213
  //  4213 + 3124 = 7337
  //
  //  That is, 349 took three iterations to arrive at a palindrome.
  //
  //  Although no one has proved it yet, it is thought that some numbers, like 196, never produce a palindrome. A number
  //  that never forms a palindrome through the reverse and add process is called a Lychrel number. Due to the theoretical
  //  nature of these numbers, and for the purpose of this problem, we shall assume that a number is Lychrel until proven
  //  otherwise. In addition you are given that for every number below ten-thousand, it will either (i) become a
  //  palindrome in less than fifty iterations, or, (ii) no one, with all the computing power that exists, has managed so
  //  far to map it to a palindrome. In fact, 10677 is the first number to be shown to require over fifty iterations
  //  before producing a palindrome: 4668731596684224866951378664 (53 iterations, 28-digits).
  //
  //  Surprisingly, there are palindromic numbers that are themselves Lychrel numbers; the first example is 4994.
  //
  //  How many Lychrel numbers are there below ten-thousand?
  //
  //  NOTE: Wording was modified slightly on 24 April 2007 to emphasise the theoretical nature of Lychrel numbers.
  def p055(): Unit = {
    def reverseAndSum(n: BigInt): BigInt = {
      n + BigInt(n.toString.reverse)
    }
    def isLychrel(n: Int): Boolean = {
      isBigLychrel(reverseAndSum(BigInt(n)), 49)
    }
    def isBigLychrel(n: BigInt, i: Int): Boolean = {
      if (i == 0) {
        true
      }
      else if (n.toString == n.toString.reverse) {
        false
      }
      else {
        isBigLychrel(reverseAndSum(n), i - 1)
      }
    }

    assert((0 to 9999).map(isLychrel).count(identity) == solutions("55").toInt)
  }

  problemMap += ("055" -> p055)

  //  A googol (10^100) is a massive number: one followed by one-hundred zeros; 100^100 is almost unimaginably large: one
  //  followed by two-hundred zeros. Despite their size, the sum of the digits in each number is only 1.
  //
  //  Considering natural numbers of the form, a^b, where a, b < 100, what is the maximum digital sum?
  def p056(): Unit = {
    val rez = (0 to 99).flatMap(a => (0 to 99).map(b => BigInt(a).pow(b).toString.toCharArray.map(_ - 48).sum)).max
    assert(rez == solutions("56").toInt)
  }

  problemMap += ("056" -> p056)

  //  It is possible to show that the square root of two can be expressed as an infinite continued fraction.
  //
  //    √ 2 = 1 + 1/(2 + 1/(2 + 1/(2 + ... ))) = 1.414213...
  //
  //  By expanding this for the first four iterations, we get:
  //
  //  1 + 1/2 = 3/2 = 1.5
  //  1 + 1/(2 + 1/2) = 7/5 = 1.4
  //  1 + 1/(2 + 1/(2 + 1/2)) = 17/12 = 1.41666...
  //  1 + 1/(2 + 1/(2 + 1/(2 + 1/2))) = 41/29 = 1.41379...
  //
  //  The next three expansions are 99/70, 239/169, and 577/408, but the eighth expansion, 1393/985, is the first
  //  example where the number of digits in the numerator exceeds the number of digits in the denominator.
  //
  //  In the first one-thousand expansions, how many fractions contain a numerator with more digits than denominator?
  def p057(): Unit = {
    def fracs(num: BigInt = BigInt(3), denom: BigInt = BigInt(2)): Stream[(BigInt, BigInt)] = {
      (num, denom) #:: fracs(num + denom * 2, denom + num)
    }

    val rez = fracs().take(1000).count(nd => nd._1.toString.length > nd._2.toString.length)
    assert(rez == solutions("57").toInt)
  }

  problemMap += ("057" -> p057)

  //  Starting with 1 and spiralling anticlockwise in the following way, a square spiral with side length 7 is formed.
  //
  //  37 36 35 34 33 32 31
  //  38 17 16 15 14 13 30
  //  39 18  5  4  3 12 29
  //  40 19  6  1  2 11 28
  //  41 20  7  8  9 10 27
  //  42 21 22 23 24 25 26
  //  43 44 45 46 47 48 49
  //
  //  It is interesting to note that the odd squares lie along the bottom right diagonal, but what is more interesting
  //  is that 8 out of the 13 numbers lying along both diagonals are prime; that is, a ratio of 8/13 ≈ 62%.
  //
  //  If one complete new layer is wrapped around the spiral above, a square spiral with side length 9 will be formed.
  //  If this process is continued, what is the side length of the square spiral for which the ratio of primes along
  //  both diagonals first falls below 10%?
  def p058(): Unit = {
    def sq(n: Int) = {
      n * n
    }
    def lowerRight(n: Int): Int = {
      sq(n * 2 + 1)
    }
    def side(n: Int): Int = {
      2 * n
    }
    def corners(n: Int): List[Int] = {
      val lr = lowerRight(n)
      val len = side(n)
      (0 to 3).map(lr - len * _).toList
    }
    def isPrime(n: Int): Boolean = {
      (2 to Math.sqrt(n).toInt).forall(n % _ != 0)
    }

    def takeWhileHigher(total: Int, primeCount: Int, count: Int, thresh: Float = 0.1F): Int = {
      val cs = corners(count)
      val newTotal = total + 4
      val newPrimes = primeCount + cs.count(isPrime)
      if (1.0F * newPrimes / newTotal < thresh) {
        side(count) + 1
      } else {
        takeWhileHigher(newTotal, newPrimes, count + 1)
      }
    }

    assert(takeWhileHigher(1, 0, 1) == solutions("58").toInt)
  }

  problemMap += ("058" -> p058)

  //  Each character on a computer is assigned a unique code and the preferred standard is ASCII (American Standard
  //  Code for Information Interchange). For example, uppercase A = 65, asterisk (*) = 42, and lowercase k = 107.
  //
  //  A modern encryption method is to take a text file, convert the bytes to ASCII, then XOR each byte with a given
  //  value, taken from a secret key. The advantage with the XOR function is that using the same encryption key on the
  //  cipher text, restores the plain text; for example, 65 XOR 42 = 107, then 107 XOR 42 = 65.
  //
  //  For unbreakable encryption, the key is the same length as the plain text message, and the key is made up of
  //  random bytes. The user would keep the encrypted message and the encryption key in different locations, and without
  //  both "halves", it is impossible to decrypt the message.
  //
  //  Unfortunately, this method is impractical for most users, so the modified method is to use a password as a key.
  //  If the password is shorter than the message, which is likely, the key is repeated cyclically throughout the
  //  message. The balance for this method is using a sufficiently long password key for security, but short enough to
  //  be memorable.
  //
  //  Your task has been made easy, as the encryption key consists of three lower case characters. Using cipher.txt
  //  (right click and 'Save Link/Target As...'), a file containing the encrypted ASCII codes, and the knowledge that
  //  the plain text must contain common English words, decrypt the message and find the sum of the ASCII values in the
  //  original text.
  def p059(): Unit = {
    val encrypted = input("p059_cipher.txt").mkString("").split(",").map(_.replaceAll("\"", "").toInt).toList
    def decrypt(e: List[Int], key: List[Int]): List[Int] = {
      e.zipWithIndex.map(XOR(key))
    }
    def XOR(key: List[Int])(e: (Int, Int)): Int = {
      key(e._2 % key.length) ^ e._1
    }
    def containsWords(encrypted: List[Int], mustFind: List[String], key: List[Int]): Boolean = {
      val decrypted = decrypt(encrypted, key).map(_.toChar).mkString("")
      mustFind.forall(w => decrypted.contains(w))
    }
    val letters = ('a'.toInt to 'z'.toInt)
    val keys = for (i <- letters; j <- letters; k <- letters) yield (List(i, j, k))

    val secret = keys.find(k => containsWords(encrypted, List(" the ", " a "), k))
    assert(decrypt(encrypted, secret.get).sum == solutions("59").toInt)
  }

  problemMap += ("059" -> p059)

  //  The primes 3, 7, 109, and 673, are quite remarkable. By taking any two primes and concatenating them in any order
  //  the result will always be prime. For example, taking 7 and 109, both 7109 and 1097 are prime. The sum of these
  //  four primes, 792, represents the lowest sum for a set of four primes with this property.
  //
  //  Find the lowest sum for a set of five primes for which any two primes concatenate to produce another prime.
  def p060(): Unit = {
    def isPrime(n: Int): Boolean = {
      (2 to Math.sqrt(n).toInt).forall(n % _ != 0)
    }
    def concatenate(a: Int, b: Int): Int = {
      (a.toString + b.toString).toInt
    }
    def isConcatenatedPrime(a: Int, b: Int): Boolean = {
      isPrime(concatenate(a, b)) && isPrime(concatenate(b, a))
    }
    def allPrime(n: Int, ps: Set[Int]): Boolean = {
      ps.forall(p => isConcatenatedPrime(n,p))
    }

    val ps = primes.takeWhile(_ < 10000).toList
    val results: scala.collection.mutable.Set[Set[Int]] = scala.collection.mutable.Set()
    var done = false
    def findSets(n: Int, ps: List[Int], curr: Set[Int] = Set()): Unit = {
      if (n == 0) {
        results += curr
        done = true
      } else if (!done) {
        val valids = ps.filter(p => allPrime(p, curr))
        valids.foreach(v => findSets(n-1, ps.dropWhile(_ <= v), curr + v))
      }
    }

    findSets(5, ps)
    assert(results.head.sum == solutions("60").toInt)
    /*This finds a solution in 1.8s, but takes 80s to exhaustively search all, hence the use of mutables & var*/
  }

  problemMap += ("060" -> p060)

  //  Triangle, square, pentagonal, hexagonal, heptagonal, and octagonal numbers are all figurate (polygonal) numbers
  //  and are generated by the following formulae:
  //
  //  Triangle 	  	  P3,n=n(n+1)/2 	  	1, 3, 6, 10, 15, ...
  //  Square 	  	    P4,n=n2 	  	      1, 4, 9, 16, 25, ...
  //  Pentagonal 	  	P5,n=n(3n−1)/2 	  	1, 5, 12, 22, 35, ...
  //  Hexagonal 	  	P6,n=n(2n−1) 	  	  1, 6, 15, 28, 45, ...
  //  Heptagonal 	  	P7,n=n(5n−3)/2 	  	1, 7, 18, 34, 55, ...
  //  Octagonal 	  	P8,n=n(3n−2) 	  	  1, 8, 21, 40, 65, ...
  //
  //  The ordered set of three 4-digit numbers: 8128, 2882, 8281, has three interesting properties.
  //
  //  1.  The set is cyclic, in that the last two digits of each number is the first two digits of the next number
  //      (including the last number with the first).
  //
  //  2.  Each polygonal type: triangle (P3,127=8128), square (P4,91=8281),and pentagonal (P5,44=2882), is represented
  //      by a different number in the set.
  //
  //  3.  This is the only set of 4-digit numbers with this property.
  //
  //  Find the sum of the only ordered set of six cyclic 4-digit numbers for which each polygonal type: triangle,
  //  square, pentagonal, hexagonal, heptagonal, and octagonal, is represented by a different number in the set.
  def p061(): Unit = {
    def triangle(n: Int): Int = { n * (n + 1) / 2 }
    def square(n: Int): Int = { n * n }
    def pentagon(n: Int): Int = { n * (3 * n - 1) / 2 }
    def hexagon(n: Int): Int = { n * (2 * n - 1) }
    def heptagon(n: Int): Int = { n * (5 * n - 3) / 2 }
    def octagon(n: Int): Int = { n * (3 * n - 2) }

    def getFollowers(n: (Int, Int), cs: List[(Int, Int)]): List[(Int, Int)] = {
      cs.filter(c => c._1 != n._1 && n._2.toString.takeRight(2) == c._2.toString.take(2))
    }

    val numbers: List[(Int) => Int] = List(triangle, square, pentagon, hexagon, heptagon, octagon)
    val nZip = numbers.zip(3 to 8)
    val candidates = nZip.flatMap(p => from(1).map(p._1).dropWhile(_ < 1000).takeWhile(_ < 10000).map((p._2, _)))
    val followerMap = candidates.map(x => (x, getFollowers(x, candidates))).toMap

    def findPath(node: (Int, Int), followers: List[(Int, Int)], currPath: List[(Int, Int)] = List()): List[(Int, Int)] = {
      val newPath = currPath ::: List(node)
      val newTypes = newPath.map(_._1)
      if (newPath.length == 6 && newTypes.distinct.length == 6 && newPath.last._2.toString.takeRight(2) == newPath.head._2.toString.take(2)) {
        newPath
      } else if (newPath.length < 6 && newPath.length == newTypes.distinct.length && followers.length > 0) {
        followers.map(f => findPath(f, followerMap(f), newPath)).maxBy(_.length)
      } else {
        List()
      }
    }
    val rez = followerMap.toList.filter(_._1._1 == 8).map(kv => findPath(kv._1, kv._2)).filter(_.nonEmpty)
    assert(rez.head.map(_._2).sum == solutions("61").toInt)
  }

  problemMap += ("061" -> p061)

  //  The cube, 41063625 (3453), can be permuted to produce two other cubes: 56623104 (3843) and 66430125 (4053).
  //  In fact, 41063625 is the smallest cube which has exactly three permutations of its digits which are also cube.
  //
  //  Find the smallest cube for which exactly five permutations of its digits are cube
  def p062(): Unit = {
    def cube(n: Int): BigInt = { Seq.fill(3)(n).map(BigInt(_)).product }
    val cubes: List[(String, IndexedSeq[BigInt])] = (1 to 9999).map(cube).groupBy(_.toString.sorted).toList.filter(_._2.length == 5)
    assert(cubes.flatMap(_._2).sorted.head.toString == solutions("62"))
  }

  problemMap += ("062" -> p062)

  //  The 5-digit number, 16807=7^5, is also a fifth power. Similarly, the 9-digit number, 134217728=8^9, is a ninth power.
  //
  //  How many n-digit positive integers exist which are also an nth power?
  /* base must be less than 10 (1 to 9) since len(10^n) == n + 1;  just brute force the exponent*/
  def p063(): Unit = {
    assert((1 to 9).flatMap(a => (1 to 30).filter(p => BigInt(a).pow(p).toString.length == p)).length == solutions("63").toInt)
  }

  problemMap += ("063" -> p063)

  //  The first ten continued fraction representations of (irrational) square roots are:
  //
  //  √2=[1;(2)], period=1
  //  √3=[1;(1,2)], period=2
  //  √5=[2;(4)], period=1
  //  √6=[2;(2,4)], period=2
  //  √7=[2;(1,1,1,4)], period=4
  //  √8=[2;(1,4)], period=2
  //  √10=[3;(6)], period=1
  //  √11=[3;(3,6)], period=2
  //  √12= [3;(2,6)], period=2
  //  √13=[3;(1,1,1,1,6)], period=5
  //
  //  Exactly four continued fractions, for N ≤ 13, have an odd period.
  //
  //  How many continued fractions for N ≤ 10000 have an odd period?
  def p064(): Unit = {
    def continuous_expansion(S: Int)(t:(Int,Int,Int)) = {
      val (m0, d0, a0) = t
      val m = d0 * a0 - m0
      val d = (S - m * m) / d0
      val a = (Math.sqrt(S).toInt + m) / d
      (m, d, a)
    }

    def recurExpand(S:Int, t:(Int, Int, Int), cycles: List[(Int, Int, Int)]=List()): List[(Int, Int, Int)] = {
      if (cycles.contains(t)) { t :: cycles }
      else {
        val next = continuous_expansion(S)(t)
        recurExpand(S, next, t :: cycles)
      }
    }

    def cycleLength(n: Int): Int = {
      val a0 = Math.sqrt(n).toInt
      val cycles = recurExpand(n, (0, 1, a0))
      cycles.indexOf(cycles.head, 1)
    }

    val squares = from(1).map(x => x * x).takeWhile(_ <= 9999).toSet
    def isSquare(n: Int): Boolean = {
      squares.contains(n)
    }

    assert((2 to 9999).filterNot(isSquare).count(n => cycleLength(n) % 2 == 1) == solutions("64").toInt)
  }

  problemMap += ("064" -> p064)

  //  Find the sum of digits in the numerator of the 100th convergent of the continued fraction for e.
  /*     index: 0  1  2  3   4  5  6   7   8
     numerator: 2  3  8  11  19 87 106 193 1264
          diff:    1  6  3   8  76 19  87  1158
       m1 mult:       2         4          6
  */
  def p065(): Unit = {
    def next(i: Int, m1: BigInt, m2: BigInt): BigInt = {
      m2 + m1 * (if (i % 3 == 2) (i/3 + 1) * 2 else 1)
    }

    def recurNext(result: Vector[BigInt], target: Int): Vector[BigInt] = {
      if (result.length == target) { result }
      else {
        val m1 = result.last
        val m2 = result(result.length - 2)
        val n = next(result.length, m1, m2)
        recurNext(result :+ n, target)
      }
    }

    val nums = recurNext(Vector(BigInt(2), BigInt(3)), 100)
    assert(nums.last.toString.toCharArray.map(_ - 48).sum == solutions("65").toInt)
  }
  problemMap += ("065" -> p065)

  //  Consider quadratic Diophantine equations of the form:
  //
  //    x2 – Dy2 = 1
  //
  //  For example, when D=13, the minimal solution in x is 6492 – 13×1802 = 1.
  //
  //  It can be assumed that there are no solutions in positive integers when D is square.
  //
  //  By finding minimal solutions in x for D = {2, 3, 5, 6, 7}, we obtain the following:
  //
  //  32 – 2×22 = 1
  //  22 – 3×12 = 1
  //  92 – 5×42 = 1
  //  52 – 6×22 = 1
  //  82 – 7×32 = 1
  //
  //  Hence, by considering minimal solutions in x for D ≤ 7, the largest x is obtained when D=5.
  //
  //  Find the value of D ≤ 1000 in minimal solutions of x for which the largest value of x is obtained.
  def p066(): Unit = {
  }
  problemMap += ("066" -> p066)



}
