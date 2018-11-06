package recfun

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
  def pascal(c: Int, r: Int): Int = c match {
    case 0 => 1
    case n if n == r => 1
    case _ => pascal(c - 1, r - 1) + pascal(c, r - 1)
  }

  /**
    * Exercise 2
    */
  def balance(chars: List[Char]): Boolean = {
    @annotation.tailrec
    def loop(chars: List[Char], depth: Int): Int = depth match {
      case -1 => -1
      case depth => chars match {
        case Nil => depth
        case '(' :: xs => loop(xs, depth + 1)
        case ')' :: xs => loop(xs, depth - 1)
        case _ :: xs => loop(xs, depth)
      }
    }

    loop(chars, 0) == 0
  }

  /**
   * Exercise 3
   */
    def countChange(money: Int, coins: List[Int]): Int = {
      def loop(money: Int, coins: List[Int]): Int = {
        if (money == 0) 1
        else coins match {
          case Nil => 0
          case x::xs if x > money => loop(money, xs)
          case x::xs => loop(money, xs) + loop(money - x, x::xs)
        }
      }

      loop(money, coins.sortWith((a,b) => a>b))
    }
  }
