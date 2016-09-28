object GameOfLife {

  def baseDef(gs: Seq[Int], n: Int): Seq[Seq[Int]] = {
    if (gs.nonEmpty) {
      require(n > 0, "must have some hunters, yo!")
    }

    Seq.empty
  }
}
