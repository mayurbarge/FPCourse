package scalaPractise

import scala.util.Random

/**
  * Created by rutvik on 26/9/16.
  */

trait SnakesAndLadders {
  val jumps : Map[Int,Int] = Map(1 -> 6, 7 -> 12, 13 -> 24, 25 -> 40)
  def move(newPos: Int, dieValue: Int) :Int= {
    if(win) {
      newPos
    }
    else{
      if(jumps.contains(newPos+dieValue))
        jumps(newPos+dieValue)
      else
        newPos+dieValue
    }
  }
  def win: Boolean
  def makeAMove: Unit
}

class BoardPlayer(val id: Int,var position: Int) extends SnakesAndLadders{
  def win: Boolean = (position >= 50)
  def makeAMove: Unit = position = move(position,(new Random()).nextInt(6) + 1)
}


object BoardGame extends App{
  val b1 = new BoardPlayer(1,1)
  val b2 = new BoardPlayer(2,1)
  do{
    b1.makeAMove
    b2.makeAMove
    println("Player Id : " + b1.id + ", Score: " + b1.position + ", Player Id : " + b2.id + ", Score: " + b2.position)
    if(b1.win)
      println("\n!!!!!Player Id: " + b1.id + ", Score: " + b1.position + " WINS!!!!!")
    if(b2.win)
      println("\n!!!!!Player Id: " + b2.id + ", Score: " + b2.position + " WINS!!!!!")
  }while(!b1.win && !b2.win)
}