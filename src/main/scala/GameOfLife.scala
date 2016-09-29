object GameOfLife {

  case class Cell(isAlive: Boolean = true) {
    def awake = Cell()

    def kill = Cell(false)

    def next(neighbours: Cell*) = {
      val alive = neighbours.count(_.isAlive)
      Cell(alive == 2 || alive == 3)
    }
  }

}
