import GameOfLife._

class GameOfLifeSpec extends BaseSpec {

  "cell" should {
    "be made alive" in {
      val cell = Cell(false)

      val result = cell.awake

      result shouldBe cell.copy(isAlive = true)
    }

    "be killed" in {
      val cell = Cell(true)

      val result = cell.kill

      result.isAlive shouldBe false
    }
  }
}

