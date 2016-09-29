object GameOfLife {

  case class Cell(isAlive: Boolean = true) {
    def awake = Cell()

    def kill = Cell(false)

    def next(neighbours: Cell*) = {
      val alive = neighbours.count(_.isAlive)
      Cell(alive == 3 || isAlive && alive == 2)
    }
  }

}
