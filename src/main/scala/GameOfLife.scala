case class Cell(isAlive: Boolean = true)

object GameOfLife {
  def awake(cell: Cell) = Cell()
  def kill(cell: Cell) = Cell(false)
}
