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
  /**
    * def getValue(col: Int, row: Int) : Int= {
    * (col, row) match {
    * case (0, 2) => 1
    * case (0, _) => 0
    * }
    * getValue(col, row-1) + getValue(col+1, row-1)
    * }
    *
    * @param c
    * @param r
    * @return
    */
   def pascal(c: Int, r: Int): Int = {
    def findPascalVal(col: Int, row: Int): Int = {
      if(row == 0) {
        (col, row) match {
          case (0, 0) => return 1
          case (_, 0) => return 0
        }
      }
      findPascalVal(col-1, row-1) + findPascalVal(col, row-1)
    }
      findPascalVal(c, r)
    }

  
  /**
   * Exercise 2
   */
    /*def balance(chars: List[Char]): Boolean = {
      def isBalanced(balanced: Boolean, openCount: Int, list: List[Char]): Boolean = {
        if(list.isEmpty) {
          if(balanced && openCount == 0)
            return true
          return false
        }

        (list.head, list.tail) match {
          case ('(', l) => return isBalanced(balanced, openCount+1, l)
          case (')', l) if(openCount > 0) => return isBalanced(balanced, openCount -1, l)
          case (')', l) if(balanced) => return isBalanced(!balanced, openCount, l)
          case _=> return isBalanced(balanced, openCount, list.tail)
        }
      }
      isBalanced(true, 0, chars)
    }*/
  def balance(chars: List[Char]): Boolean = {
    def isBalanced(balanced: Boolean, openCount: Int, list: List[Char]): Boolean = {
      if(list.isEmpty) {
        if(balanced && openCount == 0)
          return true
        return false
      }

      (list.head, list.tail) match {
        case ('(', l) => return isBalanced(balanced, openCount+1, l)
        case (')', l) if(openCount > 0) => return isBalanced(balanced, openCount -1, l)
        case (')', l) if(balanced) => return false
        case _=>
      }
      isBalanced(balanced, openCount, list.tail)
    }
    isBalanced(true, 0, chars)
  }
  
  /**
   * Exercise 3
   */

    def countChange(money: Int, coins: List[Int]): Int = {

    def countChange(remainingMoney: Int, count: Int, currentCoins: List[Int]): Int = {
      if (remainingMoney < 0)
        return count

      if (currentCoins.isEmpty) {
        if (remainingMoney == 0)
          return count + 1
        else
          return count
      }

      countChange(remainingMoney, count, currentCoins.tail) + countChange(remainingMoney - currentCoins.head, count, currentCoins)
    }
    countChange(money, 0, coins)
    }
  }
      /*def countChange(money: Int, coins: List[Int]): Int = {

      if(coins.isEmpty)
        return 0

      def isEnoughToDeduct(money: Int, amountToDeduct: Int) = {
        val remainingMoney = (money - amountToDeduct)
        remainingMoney > 0
      }

    def isFullyDeductible(money: Int, amountToDeduct: Int) = {
      val remainingMoney = money%amountToDeduct
      (remainingMoney == 0)
    }

      def countDown(count: Int, money: Int, currCoin: Int, restCoins: List[Int]): Int = {
        println("Count- " + count + " money- " + money + " restCoins- " + restCoins)
        if(money <= 0 || restCoins.isEmpty)
          return count

        if(isFullyDeductible(money, currCoin))
          return countDown(count+1, money-restCoins.head, restCoins.head, restCoins.tail)
        else
          {
           return countDown(count, money-restCoins.head, restCoins.head, restCoins.tail)
          }
      }

      countDown(0, money, coins.head, coins.tail)
    }
  }*/
