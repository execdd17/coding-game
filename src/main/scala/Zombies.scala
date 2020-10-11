import AgentFactory.AgentKind.{AgentKind, Human}

import math._
import scala.annotation.tailrec
import scala.collection.immutable.Queue
import scala.util._
import scala.io.StdIn._

object Simulation {
  case class Result(score: Double, moves: Queue[Point])

  def monteCarlo(maxRounds: Int, startingState: GameState): Result = {
    continue_sim(maxRounds, 0, startingState, Queue.empty[Point], 0.0)
  }

  @tailrec
  def continue_sim(maxRounds: Int,
                   currRound: Int,
                   currGameState: GameState,
                   moves: Queue[Point],
                   adjustedScore: Double): Result = {

    if (currRound == maxRounds) {
//      Console.err.println(s"Score:$adjustedScore. Max rounds of [$maxRounds] reached!")
      Result(adjustedScore, moves)
    } else if (currGameState.areAllHumansDead) {
//      Console.err.println(s"Score:-1. All humans are dead on round [$currRound]!")
      Result(-1, moves)
    } else if (currGameState.areAllZombiesDead) {
//      Console.err.println(s"Score:$adjustedScore. All Zombies are dead on round [$currRound]!")
      Result(adjustedScore, moves)
    } else {
      val newGameState = currGameState.advanceGameState
      val adjustedRoundScore = currGameState.getRoundScore - currGameState.cost

      continue_sim(
        maxRounds = maxRounds,
        currRound = currRound + 1,
        currGameState = newGameState,
        moves = moves :+ newGameState.getAshLocation,
        adjustedScore = adjustedScore + adjustedRoundScore)
    }
  }
}

object GameState {
  val rand = new Random(seed = 1234)
  val Array(maxX, maxY) = Array(16000, 9000)

  def getRandomPoint: Point = Point(rand.nextInt(maxX), rand.nextInt(maxY))
}

class GameState(humans: List[Agent], zombies: List[Zombie], ash: Ash) {
  val fibonacci: Array[Int] = Array(1, 2, 3, 5, 8, 13, 21, 34, 55, 89, 144, 233, 377, 610,
    987, 1597, 2584, 4181, 6765, 10946, 17711, 28657, 46368, 75025, 121393, 196418, 317811)

  val eatReward = 10

  def getAshLocation: Point = ash.point

  def areAllHumansDead: Boolean = humans.isEmpty

  def areAllZombiesDead: Boolean = zombies.isEmpty

  def advanceGameState: GameState = {
//    Console.err.println(s"Ash is at location ${ash.point}")
//    humans.foreach(Console.err.println)
//    zombies.foreach(Console.err.println)

    val humansAndAsh = humans ++ List(ash.asInstanceOf[Agent])
    val movedZombies = zombies.map(z =>
      z.moveTo(Trig.constrainedDestination(z.point, z.findClosestHuman(humansAndAsh).point, z.getMovementPoints))
    )
    val deadZombies = ash.eligibleTargets(zombies)
    val nextGenZombies = movedZombies.filterNot(z => deadZombies.exists(dz => dz.id == z.id))

    val deadHumans = nextGenZombies.foldLeft(Set.empty[Agent]) { (memo, zombie) =>
      memo ++ zombie.eligibleTargets(humans)
    }
    val nextGenHumans = humans.filterNot(h => deadHumans.exists(dh => dh.id == h.id))

    val newAsh = ash.moveTo(Trig.constrainedDestination(ash.point, GameState.getRandomPoint, ash.getMovementPoints))
    new GameState(humans=nextGenHumans, zombies=nextGenZombies, ash=newAsh)
  }

  /**
   * A zombie is worth the number of humans still alive squared x10, not including Ash.
   *
   * If several zombies are destroyed during on the same round, the nth zombie killed's worth
   * is multiplied by the (n+2)th number of the Fibonnacci sequence (1, 2, 3, 5, 8, and so on).
   * As a consequence, you should kill the maximum amount of zombies during a same turn.
   * @return
   */
  def getRoundScore: Double = {
    val zombiesToShoot = ash.eligibleTargets(zombies)
    // (num humans ** 2) * 10 * (chain #)
    zombiesToShoot.zipWithIndex.map(t => pow(humans.length, 2) * 10 * fibonacci(t._2)).sum
  }

  // Something to optimize on with more than just game score
  def cost: Double = {
    zombies.foldLeft(0) { (memo, zombie) =>
      humans.find(h => zombie.canEat(h)) match {
        case Some(_) => memo + eatReward
        case None => memo
      }
    }
  }
}

trait Movable {
  def getMovementPoints: Int
}

object Ash {
  val MovementPoints = 1000
  val GunMaxRange = 2000
}

class Ash(location: Point)
  extends Agent(-1, location) {

  override def getMovementPoints: Int = 1000

  def eligibleTargets(agents: List[Agent]): List[Agent] = agents.filter(agent => canShoot(agent))

  def canShoot(agent: Agent): Boolean = Trig.distance(agent.point, location) <= Ash.GunMaxRange

  def moveTo(point: Point): Ash = new Ash(point)

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

  def eligibleTargets(agents: List[Agent]): List[Agent] = agents.filter(agent => canEat(agent))

  def canEat(agent: Agent): Boolean = Trig.distance(point, agent.point) < 400

  def moveTo(target: Point): Zombie =
    new Zombie(id, Trig.constrainedDestination(point, target, getMovementPoints))

  def findClosestHuman(humans: List[Agent]): Agent = humans.minBy(h => Trig.distance(h.point, point))
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
    val ash = new Ash(ashStartingPoint)

    val gameState = new GameState(
      humans.values.toList,
      zombies.values.toList.asInstanceOf[List[Zombie]],
      ash
    )

    val maxIterations = 1000
    var nextMove: Point = null
    var bestScore: Double = 0.0
    var i = 0

    while (i < maxIterations) {
      val result = Simulation.monteCarlo(maxRounds = 30, startingState = gameState)

      if (result.score > bestScore) {
        bestScore = result.score
        nextMove = result.moves.dequeue._1
      }

      i += 1
    }

    // TODO: THERE IS SOMETHING WRONG WHERE IT RETURNS NULL NEXT MOVE
    Console.err.println(s"Best score found was $bestScore via point $nextMove")
    if (nextMove != null)
      println(nextMove.x + " " + nextMove.y)
    else
      println(gameState.getAshLocation.x + " " + gameState.getAshLocation.y)
  }
}