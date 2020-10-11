import org.scalatest.{Matchers, WordSpec}

class TestGameState extends WordSpec with Matchers {
  "GameState" should {
    "properly calculate the score when it is zero" in {
      val gs = new GameState(
        humans = List(new Human(0, Point(0,0))),
        zombies = List(new Zombie(1, Point(5000,5000))),
        ash = new Ash(Point(0,0)))

      gs.getRoundScore shouldBe 0
    }

    "properly calculate the score when only a single zombie is within range" in {
      val gs = new GameState(
        humans = List(new Human(0, Point(0,0))),
        zombies = List(new Zombie(1, Point(500,500))),
        ash = new Ash(Point(0,0)))

      gs.getRoundScore shouldBe 10
    }

    "properly calculate the score in complex cases" in {
      val zombies = List(
        new Zombie(1, Point(500,500)),
        new Zombie(1, Point(734,1224)),
        new Zombie(1, Point(124,333))
      )

      val gs = new GameState(
        humans = List(new Human(0, Point(0,0)), new Human(0, Point(6000,6000))),
        zombies = zombies,
        ash = new Ash(Point(0,0))
      )

      // the increasing zombie scores
      gs.getRoundScore shouldBe (40 + 80 + 120)
    }
  }
}