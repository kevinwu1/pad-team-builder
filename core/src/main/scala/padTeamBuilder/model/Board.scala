package padTeamBuilder.model

object Board {
  type BoardPositions = List[List[Boolean]]

  object BoardPositionExtensions {
    extension (b: BoardPositions) {
      def countPositions: Int = b.flatten.map(if (_) 1 else 0).reduce(_ + _)
    }
  }

  def boardPositionsToCompleteStr(positions: BoardPositions) = {
    s"${boardToStr(positions)}\n7x6:\n${boardToStr(to7x6(positions))}"
  }

  def to7x6(positions: BoardPositions): BoardPositions = {
    val p1: BoardPositions = positions.map(l => {
      l.slice(0, 4) ++ l.slice(3, 6)
    })
    p1.slice(0, 3) ++ p1.slice(2, 5)
  }

  def boardToStr(positions: BoardPositions) = {
    positions
      .map(_.map(if (_) "O" else ".").mkString("  "))
      .mkString("\n")
  }

  def boardPositionsFromBitList(allBits: List[Int]): BoardPositions = {
    if (allBits.size == 5)
      allBits.map(bits => {
        (0 to 5)
          .map(i => ((1 << i) & bits) != 0)
          .toList
      })
    else
      ???
  }
}
