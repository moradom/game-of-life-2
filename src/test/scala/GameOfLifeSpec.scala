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

    "live on to the next generation with 2 live neighbours" in {
      val cell = Cell(true)

      val result = cell.next(Cell(true), Cell(false), Cell(true))

      result.isAlive shouldBe true
    }

    "stay dead to the generation with 2 live neighbours" in {
      val cell = Cell(false)

      val result = cell.next(Cell(true), Cell(false), Cell(true))

      result.isAlive shouldBe false
    }

    "live on to the next generation with 3 live neighbours" in {
      val cell = Cell(true)

      val result = cell.next(Cell(true), Cell(false), Cell(true), Cell(true))

      result.isAlive shouldBe true
    }

    "stay dead to the generation with 3 live neighbours" in {
      val cell = Cell(false)

      val result = cell.next(Cell(true), Cell(false), Cell(true), Cell(true))

      result.isAlive shouldBe false
    }

    "die of overcrowding with more than 3 live neighbours" in {
      val cell = Cell(true)

      val result = cell.next(Cell(true), Cell(false), Cell(true), Cell(true), Cell(true))

      result.isAlive shouldBe false
    }
  }
}

