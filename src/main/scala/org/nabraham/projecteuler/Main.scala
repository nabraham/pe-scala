package org.nabraham.projecteuler

/**
 * Created by nabraham on 8/10/15.
 */
import Problems._
object Main extends scala.App {
  def timeIt(f: () => Unit): Unit = {
    val start = System.currentTimeMillis();
    f();
    val stop = System.currentTimeMillis();
    println("Finished " + f.toString() + " in " + (stop - start) + " ms")
  }

  timeIt(p001)
  timeIt(p002)
  timeIt(p003)
  timeIt(p004)
  timeIt(p005)
  timeIt(p006)
  timeIt(p007)
  timeIt(p008)
  timeIt(p009)
//  timeIt(p010)
  timeIt(p011)
}
