package recfun
import common._

import scala.collection.mutable.ArrayBuffer

object Main {
  def main(args: Array[String]) {
    println("Pascal's Triangle")
    for (row <- 0 to 10) {
      for (col <- 0 to row)
        print(pascal(col, row) + " ")
      println()
    }
  }

  /**
   * Exercise 1
   */
  def pascal(c: Int, r: Int): Int = {
    def Factorial(n: Int, res : Int = 1): Int = {
      if( n == 0)
        res
      else
        Factorial(n-1, res * n)
    }
    if(c > r || c < 0 || r < 0) 0
    else
      Factorial(r) / (Factorial(c) * Factorial(r-c))
  }

  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean =
    {
      def balanceCounter(chars: List[Char], count :Int = 0) : Boolean = {
        if (chars.isEmpty || count < 0)
          if (count == 0) true
          else false
        else if (chars.head == '(')
          balanceCounter(chars.tail, count + 1)
        else if (chars.head == ')')
          balanceCounter(chars.tail, count - 1)
        else
          balanceCounter(chars.tail, count)

      }
        balanceCounter(chars)
    }

  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = {
    if (money == 0)
      1
    else if (money < 0 || coins.isEmpty)
      0
    else
      countChange(money - coins.head, coins) + countChange(money, coins.tail)}
}

