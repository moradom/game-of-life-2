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
      def notMe(r: Int, c: Int) = (r, c) != (row + 1, col + 1)

      def nextToMe(r: Int, c: Int) = abs(row + 1 - r) <= 1 && abs(col + 1 - c) <= 1

      def copyRow(s: Seq[(Int, Int)], from: Int, to: Int) =
        s filter { case (r, c) => r == from } map { case (r, c) => (to, c) }

      def copyCol(s: Seq[(Int, Int)], from: Int, to: Int) =
        s filter { case (r, c) => c == from } map { case (r, c) => (r, to) }

      val shifted = alive.map(t => (t._1 + 1, t._2 + 1))
      val large = shifted ++
        copyRow(shifted, rows, 0) ++
        copyRow(shifted, 0, rows) ++
        copyCol(shifted, cols, 0) ++
        copyCol(shifted, 0, cols)

      large.count { case (r, c) => notMe(r, c) && nextToMe(r, c) }
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
