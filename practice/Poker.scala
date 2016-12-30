package scalaPractise

/**
  * Royal Flush
    Straight Flush
    Four of a Kind
    Full House
    Flush
    Straight
    Three of a Kind
    Two Pair
    One Pair
    High Card
  */

case class PokerCard(Suit: Char, Rank: Char)

trait Poker{

  val hand: List[String]

  require(hand.filter(( str: String) => str.length != 2) == List(),
            "Cards should only have the suit and rank, in that order")

  val pokerCards: List[PokerCard] = hand.foldLeft(List(): List[PokerCard]) ((r,c) => PokerCard(c(0),c(1)) :: r)

  val handNames = List("fourOfAKind","fullHouse","straight","flush","threeOfAKind","threeOfAKind","pair","highCard")

  val hands : Array[Boolean] = Array(fourOfAKind,fullHouse,straight,flush,threeOfAKind,threeOfAKind,pair,highCard)

  lazy val fourOfAKind : Boolean = {
    val ll = pokerCards.foldLeft("")((r,c) => r + c.Rank)
    ll.groupBy(identity).mapValues(_.size).filter(a => a._2 == 4) != Map()
  }

  lazy val fullHouse : Boolean = {
    val ll = pokerCards.foldLeft("")((r,c) => r + c.Rank)
    val lll = ll.groupBy(identity).mapValues(_.size)
    (lll.filter(a => (a._2 == 3))).size > 1 && (lll.filter(a => (a._2 == 2))).size > 1
  }

  lazy val straight : Boolean = {
    def isStraight(ll: List[Int]) : Boolean = {
      if(ll(1) == (ll(0)+1) && ll(2) == (ll(0)+2) && ll(3) == (ll(0)+3) && ll(4) == (ll(0)+4))
        true
      else if(ll(0) == 1 && ll(1) == 10 && ll(2) == 11 && ll(3) == 12 && ll(4) == 13)
        true
      else
        false
    }
    val faceToValue : Map[Char,Int] = Map('A'->1,'2'-> 2,'3'-> 3,'4'-> 4,'5'-> 5,'6'-> 6,'7'-> 7,'8'-> 8,'9'-> 9,'T'-> 10,'J'-> 11,'Q'-> 12,'K'-> 13)
    val ll = pokerCards.foldLeft(List[Int]())((r,c) => faceToValue(c.Rank) :: r).sortWith((a,b) => a < b)
    isStraight(ll)
  }

  lazy val flush : Boolean = {
    val ll = pokerCards.foldLeft("")((r,c) => r + c.Suit)
    ll.groupBy(identity).mapValues(_.size).filter(a => a._2 == 5) != Map()
  }

  lazy val threeOfAKind : Boolean = {
    val ll = pokerCards.foldLeft("")((r,c) => r + c.Rank)
    ll.groupBy(identity).mapValues(_.size).filter(a => a._2 == 3) != Map()
  }

  lazy val pair : Boolean = {
    val ll = pokerCards.foldLeft("")((r,c) => r + c.Rank)
    ll.groupBy(identity).mapValues(_.size).filter(a => a._2 == 2) != Map()
  }

  lazy val highCard: Boolean  = true

  def calcHand(counter: Int): String = {
    hands(counter) match {
      case true => handNames(counter)
      case false => calcHand(counter + 1)
    }
  }

  def getHand: String

}

case class BestHand(hand: List[String]) extends Poker{

  def getHand: String = calcHand(0)

}

object BestHand{

  def main(args: Array[String]) : Unit ={
    //println(BestHand(List("HA","H2","H3","D4","C5")).threeOfAKind())
    println(BestHand(List("HA","HJ","HJ","DA","C5")).getHand)
  }

}