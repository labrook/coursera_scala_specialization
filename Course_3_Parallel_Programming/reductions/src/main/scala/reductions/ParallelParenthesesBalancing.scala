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
    def loop(acc: Int, chars: Array[Char]): Int = {
      if ((acc < 0) || chars.isEmpty) acc
      else {
        if (chars(0) == '(') loop(acc + 1, chars.drop(1))
        else if (chars(0) == ')') loop(acc - 1, chars.drop(1))
        else loop(acc, chars.drop(1))
      }
    }

    val isBalanced = loop(0, chars)
    if (isBalanced == 0) true
    else false
  }

  /** Returns `true` iff the parentheses in the input `chars` are balanced.
   */
  def parBalance(chars: Array[Char], threshold: Int): Boolean = {

    def traverse(from: Int, until: Int, closeP: Int, openP: Int): (Int, Int) = {
      var idx = from
      var rightP = closeP
      var leftP = openP
      while (idx < until) {
        if (chars(idx) == '(') leftP += 1
        else if (chars(idx) == ')') {
          if (leftP > 0) leftP -= 1
          else rightP += 1
        }
        idx += 1
      }
      (rightP, leftP)
    }

    def reduce(from: Int, until: Int): (Int, Int) = {
      if ((until - from) <= threshold) {
        traverse(from, until, 0, 0)
      }
      else {
        val mid = from + (until - from) / 2
        val ((frontCloseP, frontOpenP), (backCloseP, backOpenP)) = parallel(reduce(from, mid), reduce(mid, until))

        if (frontOpenP > backCloseP) (frontCloseP, frontOpenP - backCloseP + backOpenP)
        else (frontCloseP + backCloseP - frontOpenP, backOpenP)
      }
    }

    reduce(0, chars.length) == (0, 0)
  }

  // For those who want more:
  // Prove that your reduction operator is associative!

}
