import scala.util.Properties._

object GameOfLife {

  case class Cell(isAlive: Boolean = true) {
    def awake = Cell()

    def kill = Cell(false)

    def next(neighbours: Cell*) = {
      val alive = neighbours.count(_.isAlive)
      Cell(alive == 3 || isAlive && alive == 2)
    }
  }

  case class Board(rows: Int, cols: Int, alive: Seq[(Int, Int)] = Seq.empty) {

    def awake(cs: (Int, Int)*) = this.copy(alive = alive ++ cs)

    def kill(cs: (Int, Int)*) = this.copy(alive = alive diff cs)

    override def toString: String = {
      def xo(c: (Int, Int)) = if (alive.contains(c)) "O" else "X"

      (for {
        r <- 0 until rows
        c <- 0 until cols
      } yield xo(r, c))
        .mkString
        .grouped(cols)
        .mkString(lineSeparator)
    }
  }

}
