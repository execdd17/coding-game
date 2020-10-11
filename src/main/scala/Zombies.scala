import AgentFactory.AgentKind.{AgentKind, Human}

import math._
import scala.collection.mutable
import scala.util._
import scala.io.StdIn._

trait Movable {
  def getMovementPoints: Int
}

object Ash {
  val MovementPoints = 1000

  def apply(location: Point, humans: Map[Int, Agent], zombies: Map[Int, Agent]): Ash = {
    new Ash(location, humans, zombies)
  }
}

class Ash(location: Point, humans: Map[Int, Agent], zombies: Map[Int, Agent])
  extends Agent(-1, location) with Movable {

  // key is the distance between the two agent ids
  // _1 is the human id and _2 is the zombie id
  private val zombieHumanDistances: Map[Double, (Int,Int)] = calculateDistances
  Console.err.println(zombieHumanDistances.toList.sortBy(_._1))

  override def getMovementPoints: Int = 1000

  def getNextMove: Point = {
    val goners = mutable.ListBuffer.empty[Int]

    val maybeMove = zombieHumanDistances.keys.toList.sorted.find { distance =>
      val canSave = isPossibleToSave(
        humans(zombieHumanDistances(distance)._1),
        zombies(zombieHumanDistances(distance)._2)
      )

      if (!canSave)
        goners += zombieHumanDistances(distance)._1

      canSave && !goners.contains(zombieHumanDistances(distance)._1)
    }

    maybeMove match {
      case Some(distanceKey) => humans(zombieHumanDistances(distanceKey)._1).point
      case None =>
        Console.err.println("IT IS HOPELESS!")
        location // stay where you are and cry
    }
  }

  private def calculateDistances: Map[Double, (Int,Int)] = {
    zombies.map { zombie =>
      val closestHuman = humans.minBy(tuple => Trig.distance(tuple._2.point, zombie._2.point))
      Map(Trig.distance(closestHuman._2.point, zombie._2.point) -> (closestHuman._1, zombie._1))
    }.flatten.toMap
  }

  private def isPossibleToSave(human: Agent, zombie: Agent): Boolean = {
    val turnsUntilDead = Trig.turnsNeededToReach(zombie.point, human.point, zombie.getMovementPoints)
    val turnsUntilRescue = Trig.turnsNeededToReach(this.location, human.point, this.getMovementPoints)

    Console.err.println(s"It will take $turnsUntilRescue turns to rescue $human and " +
      s"$turnsUntilDead turns until $zombie kills him")
    turnsUntilRescue <= turnsUntilDead
  }
}

object Trig {
  /**
   * A right triangle is created and then the pythagorean theorem is used
   * to find the hypotenuse
   * @param p1
   * @param p2
   * @return
   */
  def distance(p1: Point, p2: Point): Double = {
    sqrt(pow(p2.x - p1.x, 2) + pow(p2.y - p1.y, 2))
  }

  /**
   * @param startingPoint Where the agent starts
   * @param targetPoint Where the agent is ultimately trying to get to
   * @param movementPoints A constraint on the amount an agent can move within a single turn
   * @return The point where the agent will end up, given its movement constraint.
   */
  def constrainedDestination(startingPoint: Point, targetPoint: Point, movementPoints: Int): Point = {
    val c = Trig.distance(startingPoint, targetPoint)

    if (movementPoints >= c)
      return targetPoint

    val ratio = movementPoints/c
    val a = targetPoint.x - startingPoint.x
    val b = targetPoint.y - startingPoint.y

    Point(startingPoint.x + (a*ratio).floor.toInt, startingPoint.y + (b*ratio).floor.toInt)
  }

  /**
   * Returns the total number of turns needed for an agent to reach p2, given its movement points
   * per turn.
   * @param p1
   * @param p2
   * @param movementPoints
   * @return
   */
  def turnsNeededToReach(p1: Point, p2: Point, movementPoints: Int): Int = {
    (Trig.distance(p1, p2) / movementPoints).ceil.toInt
  }
}

case class Point(x: Int, y: Int)

abstract class Agent(val id: Int, val point: Point)
  extends Movable {
  override def toString: String = s"id:$id point:$point"
}

class Human(id: Int, point: Point) extends Agent(id, point) {
  override def getMovementPoints: Int = 0
}

class Zombie(id: Int, point: Point) extends Agent(id, point) {
  override def getMovementPoints: Int = 400
}

object AgentFactory {
  object AgentKind extends Enumeration {
    type AgentKind = Value
    val Human, Zombie = Value
  }

  def create(agentKind: AgentKind, id: Int, point: Point): Agent = {
    agentKind match {
      case AgentKind.Human => new Human(id, point)
      case AgentKind.Zombie => new Zombie(id, point)
      case other => throw new IllegalArgumentException(s"$other is not allowed")
    }
  }
}

/**
 * Save humans, destroy zombies!
 **/
object Player extends App {

  def getAgents(agentKind: AgentKind): Map[Int, Agent] = {
    val agentCount = readLine.toInt
    val agents = (0 until agentCount).foldLeft(Map.empty[Int, Agent]) { (memo, i) =>
      val Array(id, x, y) = (readLine split " ").map(_.toInt).take(3) // ignore some zombie output
      memo ++ Map(id -> AgentFactory.create(agentKind, id, Point(x, y)))
    }
    agents
  }

  // game loop
  while(true) {
    val Array(x, y) = (readLine split " ").map (_.toInt)
    val humans: Map[Int, Agent] = getAgents(AgentFactory.AgentKind.Human)
    val zombies: Map[Int, Agent] = getAgents(AgentFactory.AgentKind.Zombie)

    val ashStartingPoint = Point(x,y)
    val ash = Ash(ashStartingPoint, humans, zombies)
    val nextMove = ash.getNextMove
    println(nextMove.x + " " + nextMove.y)

    //    Console.err.println(ash.getHumanInGreatestDanger)
  }
}