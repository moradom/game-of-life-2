import scala.math._
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

    def liveNeighbours(row: Int, col: Int) = {
      def notMe(r: Int, c: Int) = (r, c) != (row, col)

      val myNeighbours = for {
        j <- - 1 to 1
        k <- - 1 to 1
      } yield (row + j, col + k)

      val myNeighboursWrapped = myNeighbours
        .map{ case (r, c) =>
          (
            if (r == -1) rows - 1 else if (r == rows) 0 else r,
            if (c == -1) cols - 1 else if (c == cols) 0 else c
            )
        }

      alive.intersect(myNeighboursWrapped).count { case (r, c) => notMe(r, c) }
    }

    def next = {
      def isAlive(r: Int, c: Int) = alive.contains((r, c))

      val matrix = for {
        r <- 0 until rows
        c <- 0 until cols
      } yield (r, c)

      val newAlive = matrix.filter { case (r, c) =>
        val count = liveNeighbours(r, c)
        count == 3 || isAlive(r, c) && count == 2
      }

      copy(alive = newAlive)
    }

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
