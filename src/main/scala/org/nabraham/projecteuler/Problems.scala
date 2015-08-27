package org.nabraham.projecteuler

import java.text.SimpleDateFormat
import java.util.Calendar

import org.nabraham.projecteuler.Common.{bigFibs, fibs, input, primes, solutions}

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
    ) yield { cal.setTime(sdf.parse("" + year + "/" +  month + "/1" )); cal.get(Calendar.DAY_OF_WEEK) }

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
    else (1 to n/2).filter(n % _ == 0).toList
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
    val cleaned = input("022.txt").mkString("").split(",").map(_.replaceAll("\"","")).sorted
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
    def from(n: Int): Stream[Int] = { n #:: from(n + 1) }
    def quadratic(a: Int, b: Int)(n: Int) = { n*n + a*n + b }
    val twoMil = primes.takeWhile(_ <= 200000).toList
    def isPrime(n: Int): Boolean = {
      twoMil.contains(n)
    }
    def quadraticPrimeLength(a: Int, b: Int): Int = {
      from(0).takeWhile(n => isPrime(quadratic(a,b)(n))).toList.length
    }
    val rez = coefs.flatMap(a => coefs.map(b => ((a,b), quadraticPrimeLength(a,b)))).maxBy(_._2)
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
    assert(1 + (2 to 501).map(n => 16*n*n - 28*n + 16).sum == solutions("28").toInt)
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
      val nums = List(a,b,c).flatMap(_.toString.toCharArray.map(_ - 48))
      nums.size == pandigits.size && nums.toSet == pandigits
    }
    def pandigital(n: Int): Boolean = {
      divisors(n).exists(d => uniqueNumbers(n/d, d, n))
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
        case Some(i) => simplify(num/i, denom/i)
        case None => (num,denom)
      }
    }

    val nums = (10 to 99)
    def isCurious(a: Int, b: Int): Boolean = {
      val aList = a.toString.toCharArray.map(_ - 48)
      val bList = b.toString.toCharArray.map(_ - 48)
      if (aList(1) == bList(1)) { false }
      else if (aList(0) == bList(1)) {
        aList(1) * b == bList(0) * a
      } else if (aList(1) == bList(0)) {
        aList(0) * b == bList(1) * a
      } else  {
        false
      }
    }

    val fracs = nums.flatMap(a => (a+1 to 99).map(b => (a,b))).filter(f => isCurious(f._1, f._2))
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
    assert((1 to 999999).filter(x => palindrome(x,2) && palindrome(x,10)).sum == solutions("36").toInt)
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
    def right(a: Int, b: Int, c: Int): Boolean = { a*a + b*b == c*c }
    def rights(p: Int): Int = {
      val tris = for (a <- 1 to p - 2;
           b <- a + 1 to p - 2;
           c = p - a - b;
           if right(a,b,c)) yield (a,b,c)
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
    def genChamp(s: Int, n: Int =1, c: Vector[Char]=Vector()): Vector[Char] = {
      if (c.size >= s) { c }
      else { genChamp(s, n+1, c ++ n.toString.toCharArray.toVector) }
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
    def isPrime(n: Int):Boolean = { (2 to Math.sqrt(n).toInt).forall(n % _ != 0) }
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
    def tri(n: Int): Int = { (n*n + n) / 2}
    val triSet = (1 to 100).map(tri).toSet
    def isTriangle(w: String): Boolean = {
      triSet.contains(w.toCharArray.map(_ - 64).sum)
    }
    val rez = input("p042_words.txt").mkString("").split(",").count(w => isTriangle(w.replaceAll("[^A-Z]","")))
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
    val rez = (0 to 9).permutations.filter(p => (2 to 8).forall(s => p.drop(s-1).take(3).mkString("").toInt % primeMap(s) == 0))
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
    def pent(n: Int): Int = { (3*n*n - n)/2 }
    def abs(n: Int): Int = { if (n >= 0) { n } else { -n } }
    val max = 10000
    val pents = (1 to max).map(pent).toSet
    val pairs = for (j <- 1 to max;
         k <- (j + 1) to max;
         pj = pent(j);
         pk = pent(k);
         d = abs(pj-pk);
         if pents.contains(pj + pk) && pents.contains(d)
    ) yield ((j,k), d)
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
    def tri(n: Long): Long = { (n*n + n) / 2 }
    def pent(n: Long): Long = { (3*n*n - n) / 2 }
    def hex(n: Long): Long = { 2*n*n - n }

    def findNext(ti: Long, pi: Long, hi: Long): (Long, Long, Long, Long) = {
      val tv = tri(ti)
      val pv = pent(pi)
      val hv = hex(hi)
      if (tv == pv && tv == hv) { (ti, pi, hi, tv) }
      else if (tv < pv && tv < hv) { findNext(ti + 1, pi, hi) }
      else if (pv < hv) { findNext(ti, pi + 1, hi) }
      else { findNext(ti, pi, hi + 1) }
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
    def from(n: Int): Stream[Int] = n #:: from(n +2)
    def isPrime(n: Int): Boolean = { p1000.contains(n) }
    def oddComposites(n: Int = 9): Stream[Int] = {
      val next = from(n+2).find(!isPrime(_))
      n #:: oddComposites(next.get)
    }
    def sq(n: Int): Int = { n*n }
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
    def from(n: Int): Stream[Int] = { n #:: from(n+1) }
    def primeFactors(n: Int, np: Int): Boolean = {
      primes.takeWhile(_ <= n/2).count(n % _ == 0) >= np
    }
    def consecutivePrimes(n: Int, np: Int): Boolean = {
      (n to (n + np - 1)).forall(i => primeFactors(i,np))
    }
    assert(from(644).find(n => consecutivePrimes(n,4)).get == solutions("47").toInt)
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
    def abs(n: Int): Int = { if (n > 0) n else -n }
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
    def takeUntilOver(s: Stream[Int], limit: Int, total: Int = 0, biggest: Int=1, size: Int = 0, count: Int=0): (Int, Int) = {
      if (limit <= 0 || s.isEmpty) { (biggest, size) }
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
    val rez = ps.map(longestWindow(_,1000000)).maxBy(_._2)
    assert(rez._1 == solutions("50").toInt)
  }
  problemMap += ("050" -> p050)

}
