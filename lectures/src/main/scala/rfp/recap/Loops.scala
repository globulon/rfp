package rfp.recap

import scala.annotation.tailrec
import scala.language.reflectiveCalls

trait Loops {
  self =>

  @tailrec
  protected final def While(cond: => Boolean)(exec: => Unit): Unit =
    if (cond) {
      exec
      While(cond)(exec)
    } else ()

  @tailrec
  protected final def Repeat1(exec: => Unit)(cond: => Boolean): Unit = {
    exec
    if (cond) ()
    else Repeat1(exec)(cond)
  }

  def Repeat(exec: => Unit) = new {
    @tailrec
    def loop(exec: => Unit)(cond: => Boolean): Unit = {
      exec
      if (cond) ()
      else loop(exec)(cond)
    }

    def Until(cond: => Boolean): Unit = loop(exec)(cond)
  }
}

object TestLoops extends Loops {
  def main(args: Array[String]) {
    Repeat {
      println("yaye!")
    } Until (true)
  }
}
