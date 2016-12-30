import scala.collection.mutable
import scala.util.Random

case class Snake(downStep: Int)
case class Ladder(upStep: Int)
case class Cell(cellNumber: Int, snake: Option[Snake] = None, ladder: Option[Ladder] = None)

object Dice {def roll: Int = Random.nextInt(6)+1 }

class Player(name:String)

class Board {
  private val Max = 40
  private val startPosition = 0
  val playerPositions = new mutable.HashMap[Player, Int]()

  val snakeOrLadderCells:Set[Cell] = {
    Set[Cell](
      Cell(5,Some(Snake(2))),
      Cell(8,Some(Snake(3))),
      Cell(15,Some(Snake(10))),
      Cell(23,Some(Snake(17))),
      Cell(29,Some(Snake(21))),
      Cell(38,Some(Snake(25))),

      Cell(3,None,Some(Ladder(10))),
      Cell(6,None,Some(Ladder(19))),
      Cell(13,None,Some(Ladder(20))),
      Cell(27,None,Some(Ladder(32))),
      Cell(32,None,Some(Ladder(38)))
    )
  }

  def movePlayer(player: Player, position: Int) = {
    val newPlayerPosition = playerPositions.get(player).getOrElse(startPosition)  + position
    val snakeOrLadderCell = snakeOrLadderCells.find(_.cellNumber == newPlayerPosition)
    snakeOrLadderCell match {
      case Some(cell) => stepDownOrStepUp(player, cell)
      case None => moveForeward(player, newPlayerPosition)
    }
  }

  def isWinner(player: Player) = playerPositions.get(player) match {
    case Some(playerPosition) => playerPosition >= Max
    case None => false
  }

  private def moveForeward(player: Player, newPlayerPosition: Int) = {
    println("Moving to -> " + newPlayerPosition)
    playerPositions.update(player, newPlayerPosition)
  }

  private def stepDownOrStepUp(player: Player, cell: Cell) = {
    cell.snake.map(
      snake => {
        println("Bitten by snake ~~~~~~~~(..)  ..taking down to " + snake.downStep)
        playerPositions.update(player, snake.downStep)
      }
    )
    cell.ladder.map(
      ladder => {
        println(":) Climbing up ladder =======  to " + ladder.upStep)
        playerPositions.update(player, ladder.upStep)
      }
    )
  }
}

object SinglePlayerGame {
  def main(args: Array[String]): Unit = {
    val player = new Player("mayur")
    val board = new Board

    while(!board.isWinner(player)) {
      val dice = Dice.roll
      //     println("Dice - " + dice)
      board.movePlayer(player, dice)
    }

  }
}

