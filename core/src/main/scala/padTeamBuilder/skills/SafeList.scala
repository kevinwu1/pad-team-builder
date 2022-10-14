package padTeamBuilder.skills

final case class SafeList(arr: List[Int]) {
  def apply(i: Int) = {
    try {
      arr(i)
    } catch {
      case e: IndexOutOfBoundsException => 0
    }
  }

  def slice(i: Int, j: Int) =
    (if (arr.size < j) arr ++ List.fill(j)(0) else arr).slice(i, j)
  def indexOf(i: Int) = arr.indexOf(i)
  def map[B](f: Int => B): List[B] = arr.map(f)
  def grouped = arr.grouped

  override def toString = arr.toString
}
