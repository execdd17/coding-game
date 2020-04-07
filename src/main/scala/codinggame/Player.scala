import math._
import scala.util._
import scala.io.StdIn._

/**
 * Auto-generated code below aims at helping you parse
 * the standard input according to the problem statement.
 **/
object Player extends App {
  val Array(width, height, myId) = (readLine split " ").map (_.toInt)

  // TODO: Create Terrain Matrix
  for(i <- 0 until height) {
      val line = readLine
      Console.err.println(line)
  }

  // Write an action using println
  // To debug: Console.err.println("Debug messages...")

  // NOTE: We have to specify our starting coordinate
  println("7 14")

  // game loop
  while(true) {
      val Array(x, y, myLife, oppLife, torpedoCooldown, sonarCooldown, silenceCooldown, mineCooldown) = (readLine split " ").map (_.toInt)
      val sonarResult = readLine
      val opponentOrders = readLine

      // Write an action using println
      // To debug: Console.err.println("Debug messages...")
      Console.err.println(sonarResult)
      Console.err.println(opponentOrders)
      Console.err.println(x,y,myLife, oppLife, torpedoCooldown, sonarCooldown, silenceCooldown, mineCooldown)
      println("MOVE N TORPEDO")
  }
}