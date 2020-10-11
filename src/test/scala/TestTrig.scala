import org.scalatest.{Matchers, WordSpec}

class TestTrig extends WordSpec with Matchers {
  "Trig" should {
    "properly calculate the distance between two points" in {
      Trig.distance(Point(0,0), Point(0,0)) shouldBe 0
      Trig.distance(Point(0,0), Point(500,500)).floor shouldBe 707
      Trig.distance(Point(0,0), Point(-500,-500)).floor shouldBe 707
      Trig.distance(Point(100,100), Point(300,300)).floor shouldBe 282
      Trig.distance(Point(100,100), Point(-100,300)).floor shouldBe 282
      Trig.distance(Point(200,100), Point(200,200)).floor shouldBe 100
      Trig.distance(Point(3,5), Point(3,2)) shouldBe 3.0
    }

    "return the correct point when constrained by movement points" in {
      val point = Trig.constrainedDestination(Point(0,0), Point(500,500), 400)
      point shouldEqual Point(282, 282)

      val point2 = Trig.constrainedDestination(Point(0,0), Point(800,800), 1000)
      point2 shouldEqual Point(707, 707)

      val point3 = Trig.constrainedDestination(Point(100,100), Point(900,900), 1000)
      point3 shouldEqual Point(807, 807)

      val point4 = Trig.constrainedDestination(Point(8250,7399), Point(8250,4500), 400)
      point4 shouldEqual Point(8250, 6999)

      val point5 = Trig.constrainedDestination(Point(100,100), Point(200,100), 400)
      point5 shouldEqual Point(200,100)
    }

    "return the correct number of turns to reach a given point" in {
      Trig.turnsNeededToReach(Point(0,0), Point(600,600), 1000) shouldBe 1
      Trig.turnsNeededToReach(Point(0,0), Point(800,800), 1000) shouldBe 2
      Trig.turnsNeededToReach(Point(0,0), Point(800,800), 400) shouldBe 3
    }
  }
}