import GameOfLife._

class GameOfLifeSpec extends BaseSpec {

  "cell on board" should {
    "be made alive" in {
      val board = Board(1, 1)

      val result = board.awake((0, 0))

      result.isAlive(0, 0) shouldBe true
    }

    "be killed" in {
      val board = Board(1, 1).awake((0, 0))

      val result = board.kill((0, 0))

      result.isAlive(0, 0) shouldBe false
    }

    "die with fewer than 2 live neighbours" in {
      val board = Board(2, 2).awake((0, 0), (1,1))

      val result = board.next

      result.isAlive(0, 0) shouldBe false
    }

    "live on to the next generation with 2 live neighbours" in {
      val board = Board(3, 3).awake((0, 0), (1,1), (2,2))

      val result = board.next

      result.isAlive(1, 1) shouldBe true
    }

    "stay dead to the generation with 2 live neighbours" in {
      val board = Board(3, 3).awake((0, 0), (2,2))

      val result = board.next

      result.isAlive(1, 1) shouldBe false
    }

    "live on to the next generation with 3 live neighbours" in {
      val board = Board(3, 3).awake((0, 0), (0,1), (1,0), (1,1))

      val result = board.next

      result.isAlive(1, 0) shouldBe true
    }

    "come alive with exactly 3 live neighbours" in {
      val board = Board(3, 3).awake((0, 0), (0,1), (1,1))

      val result = board.next

      result.isAlive(1, 0) shouldBe true
    }

    "die of overcrowding with more than 3 live neighbours" in {
      val board = Board(3, 3).awake((0, 0), (0,1), (1,0), (1,1), (1,2))

      val result = board.next

      result.isAlive(0, 1) shouldBe false
    }
  }

  "board" should {
    "start with 2 dimensions of dead cells" in {
      val result = Board(2, 3)

      result.toString shouldBe """XXX
                                 |XXX""".stripMargin
    }

    "be able to awake cells by position" in {
      val board = Board(2, 3)

      val result = board.awake((1,2)).awake((1,0), (0,2))

      result.toString shouldBe """XXO
                                 |OXO""".stripMargin
    }

    "be able to kill cells by position" in {
      val board = Board(2, 3)

      val result = board.awake((1,2)).awake((1,0), (0,2)).kill((1,0), (1,2))

      result.toString shouldBe """XXO
                                 |XXX""".stripMargin
    }

    "return all the neighbours of cell given by position w/out wrap" in {
      // X X O X
      // O[O]O X
      // X O X O
      val board = Board(3, 4)
        .awake((1,1)) // self, excluded
        .awake((1,2), (1,0), (0,2), (2,1))
        .awake((2,3)) // excluded

      val result = board.liveNeighbours(1, 1)

      result shouldBe 4
    }

    "return all the neighbours of cell given by position with wrap" in {
      //[X]X X O
      // O X O O
      // X O X X
      val board = Board(3, 4)
        .awake((0,3), (1,3), (1,0), (2,1))
        .awake((1,2)) // excluded

      val result = board.liveNeighbours(0, 0)

      result shouldBe 4
    }

    "wrap around the edges" in {
      // X X O
      // O X O
      // X X X
      val board = Board(3, 3).awake((1,2), (1,0), (0,2))

      val result = board.next

      result.toString shouldBe """OOO
                                 |OOO
                                 |OOO""".stripMargin
      /*
         X   X X X   X

         O   X X O   X
         O   O X O   O
         X   X X X   X

         O   X X O   X
      */
      /*
         X   X X X   X

         O   O O O   X
         O   O O O   O
         X   X X O   X

         O   X X O   X
      */

    }

    "wrap around the edges on a board with only 1 dead cell" in {
      val board = Board(1, 1)

      val result = board.next

      result.toString shouldBe """X""".stripMargin
    }
  }

  "wrap around the edges on a board with only 1 alive cell" in {
    val board = Board(1, 1).awake((0, 0))

    val result = board.next

    result.toString shouldBe """X""".stripMargin
  }

}

