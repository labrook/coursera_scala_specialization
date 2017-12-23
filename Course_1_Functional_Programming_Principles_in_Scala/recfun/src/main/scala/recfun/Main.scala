package recfun

object Main {
  def main(args: Array[String]) {
    println("Pascal's Triangle")
    for (row <- 0 to 10) {
      for (col <- 0 to row)
        print(pascal(col, row) + " ")
      println()
    }

    // assert(!balance(":-)".toList), ":-) is not balanced")
    // assert(!balance("())(".toList), "())( is not balanced")
    if(!balance(":-)".toList)) println(":-) is not balanced. Passed test!")
    if(!balance("())(".toList)) println("())( is not balanced. Passed test!")

    println("Ways to break 4 using {2, 1} = " + countChange(4, List(1, 2))) // Pass
    println("Ways to break 4 using {1, 2} = " + countChange(4, List(1, 2))) // Pass

    println("Breaking 300 with unsorted changes = " + countChange(300,List(5,50,100,20,200,10)))
    println("Breaking 300 with unsorted changes = " + countChange(300,List(5,10,20,50,100,200)))
  }

  /**
   * Exercise 1
   */
    def pascal(c: Int, r: Int): Int = {
      if (c == 0 || c == r) 1
      else pascal(c-1, r-1) + pascal(c, r-1)
    }
  
  /**
   * Exercise 2
   */
    def balance(chars: List[Char]): Boolean = {

      def loop(acc: Int, chars: List[Char]): Int = {
        if ((acc < 0) || (chars.isEmpty)) acc
        else {
          if (chars.head == '(') loop(acc + 1, chars.tail)
          else if (chars.head == ')') loop(acc - 1, chars.tail)
          else loop(acc, chars.tail)
        }
      }

      val isBalanced = loop(0, chars)
      if (isBalanced == 0) true
      else false
    }
  
  /**
   * Exercise 3
   */
    def countChange(money: Int, coins: List[Int]): Int = {

      def countChangeHelper(coins: List[Int]): Int = {
        if ((money > 0) && (!coins.isEmpty)) {
          if (money == coins.head) 1
          else countChange(money - coins.head, coins) +
            // countChange(money - coins.head, coins.tail) +
            countChange(money, coins.tail)
        }
        else 0
      }

      val sortedCoins = coins.sortWith(_ < _)
      countChangeHelper(sortedCoins)
    }
  }
