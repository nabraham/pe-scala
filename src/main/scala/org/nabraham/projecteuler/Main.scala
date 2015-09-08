package org.nabraham.projecteuler

/**
 * Created by nabraham on 8/10/15.
 */
import Problems.problemMap
object Main extends scala.App {
  def timeIt(s: String, f: () => Unit): Unit = {
    val start = System.currentTimeMillis();
    f()
    val stop = System.currentTimeMillis();
    println("Finished " + s + " in " + (stop - start) + " ms")
  }

//  val takeTooLong = "010,012,023,027,031,035,039,043,044,046,047,050,051,052".split(",")
//  problemMap.keys.toList.sorted.foreach(key => if (!takeTooLong.contains(key)) {
//    timeIt(key, problemMap(key))
//  })
  timeIt("054", problemMap("054"))
}
