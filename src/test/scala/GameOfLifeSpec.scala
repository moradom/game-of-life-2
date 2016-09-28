import GameOfLife._

class GameOfLifeSpec extends BaseSpec {

  "cell" should {
    "be made alive" in {
      val cell = Cell()

      val result = awake(cell)

      result.isAlive shouldBe true
    }

    "be killed" in {
      val cell = Cell(true)

      val result = kill(cell)

      result.isAlive shouldBe false
    }
  }
}

