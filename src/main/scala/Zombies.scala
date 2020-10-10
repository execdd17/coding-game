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

  private val humanZombieDistances = calculateDistances
  Console.err.println(humanZombieDistances)

  override def getMovementPoints: Int = 1000

  def getNextMove: Point = {
    val maybeMove = humanZombieDistances.keys.toList.sorted.find { distance =>
      isPossibleToSave(humans(humanZombieDistances(distance)),zombies.head._2)
    }

    maybeMove match {
      case Some(distanceKey) => humans(humanZombieDistances(distanceKey)).point
      case None =>
        Console.err.println("IT IS HOPELESS!")
        location // stay where you are and cry
    }
  }

  private def calculateDistances: Map[Double, Int] = {
    humans.map { human =>
      val closestZombie = zombies.minBy(tuple => Trig.distance(tuple._2.point, human._2.point))
      Map(Trig.distance(closestZombie._2.point, human._2.point) -> human._1)
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
    val rise = abs(p2.y - p1.y)
    val run = abs(p2.x - p1.x)

    sqrt(pow(rise, 2) + pow(run, 2))
  }

  /**
   * This creates an isosceles right triangle where two sides are equal,
   * two angles are equal (45 degrees), the third angle is 90, and the base is computed
   *
   * NOTE: The base length is rounded down in order to keep x any y Integers
   * @param startingPoint Where the agent starts
   * @param targetPoint Where the agent is ultimately trying to get to
   * @param movementPoints A constraint on the amount an agent can move within a single turn
   * @return The point where the agent will end up, given its energy constraint.
   */
  // TODO: THIS HAS A BUG I THINK IN IT BECAUSE THE METHOD BELOW DOESN"T WORK RIGHT@@@
  def constrainedDestination(startingPoint: Point, targetPoint: Point, movementPoints: Int): Point = {
    val legLength = sqrt(pow(movementPoints, 2) / 2)
    Point(x = legLength.floor.toInt, y = legLength.floor.toInt)
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
    val totalDistance = Trig.distance(p1, p2)
//    val distanceTraveledPerTurn = Trig.distance(p1, Trig.constrainedDestination(p1, p2, movementPoints))

//    Console.err.println(s"Comparing $p1 to $p2 with $movementPoints movementPoints. " +
//      s"totalDistance:$totalDistance distanceTraveledPerTurn:$distanceTraveledPerTurn")
    (totalDistance / movementPoints).ceil.toInt
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