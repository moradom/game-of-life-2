object GameOfLife {

  case class Cell(isAlive: Boolean = true) {
    def awake = Cell()

    def kill = Cell(false)

    def next(neighbours: Cell*) =
      if (neighbours.count(_.isAlive) == 1) Cell(false) else Cell(true)
  }

}
