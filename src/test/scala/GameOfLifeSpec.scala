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

    "die with fewer than 2 live neighbours" in {
      val cell = Cell(true)

      val result = cell.next(Cell(true), Cell(false), Cell(false))

      result.isAlive shouldBe false
    }
  }
}

