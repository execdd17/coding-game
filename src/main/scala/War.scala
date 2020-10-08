import scala.annotation.tailrec
import scala.collection.mutable
import scala.io.StdIn._
import scala.math._

class Cards(val queue: mutable.Queue[Card]) {
  def add(newCards: Card*): Unit = newCards.foreach(queue.enqueue(_))

  def take(amount: Int): List[Card] =
    1.to(amount).foldLeft(List.empty[Card])((memo, i) => memo ++ List(queue.dequeue()))

  override def toString: String = queue.toString()
}

class Player {
  val cards: Cards = new Cards(mutable.Queue.empty[Card])

  def addCards(newCards: Card*): Unit = cards.add(newCards:_*)

  def presentCard: Card = cards.take(1).head

  def hasCards: Boolean = cards.queue.nonEmpty

  override def toString: String = cards.toString
}

object Card {
  val ValueMap: Map[Char, Int] = Map('J' -> 11, 'Q' -> 12, 'K' -> 13, 'A' -> 14)

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

  @tailrec
  def resolve(p1: Player, p1Cards: Cards, p2: Player, p2Cards: Cards): Unit = {
    val p1Card = p1Cards.queue.last
    val p2Card = p2Cards.queue.last

    // NOTE: this order is critical and specified by game rules
    val awardedCards = p1Cards.queue.toList ++ p2Cards.queue.toList

    if (p1Card > p2Card) {
      player1.addCards(awardedCards:_*)
    } else if (p1Card < p2Card) {
      player2.addCards(awardedCards: _*)
    } else {
      1.to(4).foreach(i => p1Cards.add(p1.presentCard))
      1.to(4).foreach(i => p2Cards.add(p2.presentCard))
      resolve(player1, p1Cards, player2, p2Cards)
    }
  }

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

  var numRounds = 0

  while (player1.hasCards && player2.hasCards) {
    val p1Cards = new Cards(mutable.Queue(player1.presentCard))
    val p2Cards = new Cards(mutable.Queue(player2.presentCard))

    resolve(player1, p1Cards, player2, p2Cards)
    numRounds += 1
  }

  // Write an answer using println
  // To debug: Console.err.println("Debug messages...")

  if (player1.hasCards && player2.hasCards)
    println("PAT")
  else if (player1.hasCards)
    println(s"1 $numRounds")
  else
    println(s"2 $numRounds")

}