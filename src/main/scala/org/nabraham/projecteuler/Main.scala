package org.nabraham.projecteuler

/**
 * Created by nabraham on 8/10/15.
 */
import Problems._
object Main extends scala.App {
  def timeIt(s: String, f: () => Unit): Unit = {
    val start = System.currentTimeMillis();
    f();
    val stop = System.currentTimeMillis();
    println("Finished " + s + " in " + (stop - start) + " ms")
  }

  timeIt("001", p001)
  timeIt("002", p002)
  timeIt("003", p003)
  timeIt("004", p004)
  timeIt("005", p005)
  timeIt("006", p006)
  timeIt("007", p007)
  timeIt("008", p008)
  timeIt("009", p009)
//  timeIt("010", p010)
  timeIt("011", p011)
  timeIt("012", p012)
  timeIt("013", p013)
  timeIt("014", p014)
  timeIt("015", p015)
  timeIt("016", p016)
  timeIt("017", p017)
  timeIt("018", p018)
  timeIt("019", p019)
}
