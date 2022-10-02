package model

object Board {

  def boardPositionToCompleteStr(positions: List[List[Boolean]]) = {
    s"${boardToStr(positions)}\n7x6:\n${boardToStr(to7x6(positions))}"
  }

  def to7x6(positions: List[List[Boolean]]): List[List[Boolean]] = {
    val p1: List[List[Boolean]] = positions.map(l => {
      l.slice(0, 4) ++ l.slice(3, 6)
    })
    p1.slice(0, 3) ++ p1.slice(2, 5)
  }

  def boardToStr(positions: List[List[Boolean]]) = {
    positions
      .map(_.map(if (_) "O" else ".").mkString("  "))
      .mkString("\n")
  }

  def boardPositionsFromBitList(allBits: List[Int]): List[List[Boolean]] = {
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
