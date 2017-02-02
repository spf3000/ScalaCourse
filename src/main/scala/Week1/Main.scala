package Week1

object Main {
  def main(args: Array[String]) {
    println("Pascal's Triangle")
    for (row <- 0 to 10) {
      for (col <- 0 to row)
        print(pascal(col, row) + " ")
      println(" ")
    }

    val chars = "I told him (that it’s not (yet) done). (But he wasn’t listening)".toCharArray().toList
    println("balance", balance(chars))

    val list = List(1,2)

  println("count change ", countChange(6, list))

  }

  /**
    * Exercise 1
    */
  def pascal(c: Int, r: Int): Int = if (c == 0 || c == r) 1 else pascal(c - 1, r - 1) + pascal(c, r - 1)

  /**
    * Exercise 2
    */
  def balance(chars: List[Char]): Boolean = {
    def evaluate(num: Int): Boolean = {
      if (num == 0) true else false
    }

    def loop(score: Int, chars: List[Char]): Boolean = {
      if (score < 0) return false
      if (!chars.isEmpty) {
        if (chars.head.equals('(')) loop(score + 1, chars.tail)
        else if (chars.head.equals(')')) loop(score - 1, chars.tail)
        else loop(score, chars.tail)
      }
      else evaluate(score)
    }

    loop(0, chars)
  }

  /**
    * Exercise 3
    */
  def countChange(money: Int, coins: List[Int]): Int = {
    def loop(money: Int, coins: List[Int], acc: Int): Int = {
      def countByCoin(head: Int, tail: List[Int], subtValue: Int, acc: Int): Int = {
        if (tail.isEmpty) {
          if (money % head == 0) acc + 1 else acc
        }
        else {
          if (subtValue > 0) {
            if (subtValue % head  == 0)
              countByCoin(head, tail, subtValue - tail.head, acc + 1)
            else
              countByCoin(head, tail, subtValue - tail.head, acc)
          }
          else
            countByCoin(head, tail.tail, money, acc)
        }
      }
        if (coins.tail.isEmpty) acc
        else{
          loop(money, coins.tail, acc + countByCoin(coins.head, coins.tail, money-coins.head, 0))

        }
    }
    loop(money, coins, 0)
  }
}
