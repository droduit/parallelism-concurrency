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
    def balanceIter(countBalancing:Int, list : Array[Char]):Boolean = {
      if(countBalancing<0 || list.isEmpty) countBalancing == 0
      else {
        val c = list.head
        if(c=='(') balanceIter(countBalancing+1, list.tail)
        else if(c==')') balanceIter(countBalancing-1, list.tail)
        else  balanceIter(countBalancing, list.tail)
      }
    }
    
    balanceIter(0,chars)
  }

  /** Returns `true` iff the parentheses in the input `chars` are balanced.
   */
  def parBalance(chars: Array[Char], threshold: Int): Boolean = {

    // Sequential traversal part : tailrec ou while loop
    @tailrec
    def traverse(idx: Int, until: Int, open: Int, close: Int) : (Int, Int) = {
      if(idx>=until) (open, close)
      else {
        chars(idx) match {
          case '(' =>
            traverse(idx+1, until, open+1, close)
          case ')' =>
            if(open == 0) traverse(idx+1, until, open, close+1)
            else traverse(idx+1, until, open - 1, close)
          case _ =>
            traverse(idx+1, until, open, close)
        }
      }
    }
    
    // Parallel reduction
    def reduce(from: Int, until: Int) : (Int, Int) = {
      if(until - from <= threshold)
        traverse(from, until, 0, 0)
       else {

         val mid = from + (until-from)/2
         
         val ((open1, close1), (open2, close2)) = parallel(
           reduce(from, mid),
           reduce(mid, until)
         )
         
         val completed = open1 - close2;
        (open2 + math.max(completed, 0), close1 - math.min(completed, 0))
         //(open1 - math.max(close2,0), close1 + math.min(open2,0))
         /*
         if(open1 - close2 == 0)
           (close1, open2)
          else 
            (1,-1)
            *
            */
         //(completed - math.min(close2, 0), completed - math.min(open2,0))
       }
    }

      reduce(0, chars.length) == (0,0)
  }

  // For those who want more:
  // Prove that your reduction operator is associative!

}
