package codinggame

import math._
import scala.collection.mutable
import scala.util._
import scala.io.StdIn._


class Cards(val queue: mutable.Queue[Card]) {
  def add(newCards: Card*): Unit = newCards.foreach(queue.enqueue(_))

  def take(amount: Int): List[Card] =
    1.to(amount).foldLeft(List.empty[Card])((memo, i) => memo ++ List(queue.dequeue()))

}

class Player {
  private val cards: Cards = new Cards(mutable.Queue.empty[Card])

  def addCards(newCards: Card*): Unit = cards.add(newCards:_*)

  def presentCard: Card = cards.take(1).head
}

object Card {
  val ValueMap: Map[Char, Int] = Map('J' -> 11, 'Q' -> 12, 'K' -> 12, 'A' -> 13)

  def apply(token: String): Card =
    new Card(value = token.substring(0, token.length - 1), token.last)
}

class Card(value: String, suit: Char) extends Ordered[Card] {
  def numericValue: Int = {
    value match {
      case v if v.forall(_.isDigit) => v.toInt
      case v if v.head.isLetter =>
        Card.ValueMap.getOrElse(v.head, throw new IllegalArgumentException(s"$v is invalid"))
      case v => throw new IllegalArgumentException(s"Unexpected value $v")
    }
  }

  override def toString: String = s"$value$suit"

  override def compare(other: Card): Int = this.numericValue - other.numericValue
}

/**
 * Auto-generated code below aims at helping you parse
 * the standard input according to the problem statement.
 **/
object Solution extends App {
  val player1 = new Player
  val player2 = new Player

  val n = readLine.toInt // the number of cards for player 1
  for (i <- 0 until n) {
    val cardp1 = readLine // the n cards of player 1
    player1.addCards(Card.apply(cardp1))
  }

  val m = readLine.toInt // the number of cards for player 2
  for (i <- 0 until m) {
    val cardp2 = readLine // the m cards of player 2
    player2.addCards(Card.apply(cardp2))
  }

  println(player1.presentCard)

  // Write an answer using println
  // To debug: Console.err.println("Debug messages...")

  println("PAT")
}