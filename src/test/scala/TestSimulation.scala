import org.scalatest.{Matchers, WordSpec}

class TestSimulation extends WordSpec with Matchers {
  "Simulation" should {
    "run in its most simple way" in {
      val gs = new GameState(
        humans = List(new Human(0, Point(0,0))),
        zombies = List(new Zombie(1, Point(500,500))),
        ash = new Ash(Point(0,0)))

      Simulation.monteCarlo(maxRounds = 1, startingState = gs) shouldBe 10
    }

    "run monte carlo" in {
      val gs = new GameState(
        humans = List(new Human(0, Point(0,0))),
        zombies = List(new Zombie(1, Point(2500,2500)), new Zombie(2, Point(3000,3000)), new Zombie(2, Point(2800,2800))),
        ash = new Ash(Point(5000,5000)))

      var score = 0.0
      var generation = 0
      while (score <= 30) {
        if (generation % 100000 == 0)
          Console.err.println("Generation " + generation)

        score = Simulation.monteCarlo(maxRounds = 10, startingState = gs)
        generation += 1
      }
    }
  }
}