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
    def loop(money: Int, coins: List[Int], iter: Int): Int = {
      if (money < 0) 0
      else if (money == 0) 1
      else if (iter <= 0  && money > 0) 0
      else  loop( money , coins, iter -1 ) + loop(money - coins(iter-1) , coins, iter);
    }
    loop(money, coins, coins.size)
  }
}
