package reductions

import scala.annotation._
import org.scalameter._
import common._

object ParallelParenthesesBalancingRunner {

  @volatile var seqResult = false

  @volatile var parResult = false

  val standardConfig = config(
    Key.exec.minWarmupRuns -> 40,
    Key.exec.maxWarmupRuns -> 80,
    Key.exec.benchRuns -> 120,
    Key.verbose -> true
  ) withWarmer(new Warmer.Default)

  def main(args: Array[String]): Unit = {
    val length = 100000000
    val chars = new Array[Char](length)
    val threshold = 10000
    val seqtime = standardConfig measure {
      seqResult = ParallelParenthesesBalancing.balance(chars)
    }
    println(s"sequential result = $seqResult")
    println(s"sequential balancing time: $seqtime ms")

    val fjtime = standardConfig measure {
      parResult = ParallelParenthesesBalancing.parBalance(chars, threshold)
    }
    println(s"parallel result = $parResult")
    println(s"parallel balancing time: $fjtime ms")
    println(s"speedup: ${seqtime / fjtime}")
  }
}

object ParallelParenthesesBalancing {

  /** Returns `true` iff the parentheses in the input `chars` are balanced.
   */
  def balance(chars: Array[Char]): Boolean = {
    def loop(a: Array[Char], balance: Int, index: Int): Boolean = {
      if (index >= a.length) balance == 0
      else if (a(index) == ')' && balance <= 0) false
      else loop(a, balance + eval(a(index)), index + 1)
    }

    def eval(c: Char): Int = {
      if (c == '(') 1
      else if (c == ')') -1
      else 0
    }

    if (chars.length == 0) true else loop(chars, 0, 0)
  }

  /** Returns `true` iff the parentheses in the input `chars` are balanced.
   */
  def parBalance(chars: Array[Char], threshold: Int): Boolean = {

    def traverse(idx: Int, until: Int, left: Int, right: Int): (Int, Int) = {
      if (idx >= until) (left, right)
      else chars(idx) match {
        case '(' => traverse(idx + 1, until, left + 1, right)
        case ')' =>
          val (l, r) = if (left == 0) (left, right + 1)
                       else (left - 1, right)
          traverse(idx + 1, until,  l, r)
        case _ => traverse(idx + 1, until, left, right)
      }
    }

    def reduce(from: Int, until: Int): (Int, Int) = {
      if (until - from <= threshold) traverse(from, until, 0, 0)
      else {
        val mid = (from + until) / 2
        val ((l1, r1), (l2, r2)) = parallel(reduce(from, mid), reduce(mid, until))
        if (l1 >= r2) (l1 - r2 + l2, r1)
        else (l2, r1 + r2 - l1)
      }
    }

    reduce(0, chars.length) == (0, 0)
  }

  // For those who want more:
  // Prove that your reduction operator is associative!

}
