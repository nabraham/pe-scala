/**
 * Created by nabraham on 8/8/15.
 */
import Problems._;

object ProjectEuler extends scala.App {

  def timeIt(f: () => Unit): Unit = {
    val start = System.currentTimeMillis();
    f();
    var stop = System.currentTimeMillis();
    println("Finished " + f.toString() + " in " + (stop - start) + " ms")
  }

  timeIt(p001)
  timeIt(p002)
  timeIt(p003)
  timeIt(p004)
  timeIt(p005)
  timeIt(p006)
  timeIt(p007)
}
