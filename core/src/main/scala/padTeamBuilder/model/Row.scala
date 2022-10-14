package padTeamBuilder.model

enum Row(desc: String) extends Enum[Row] {
  case T extends Row("top")
  case T2 extends Row("2nd from the top")
  case C extends Row("center")
  case B2 extends Row("2nd from the bottom")
  case B extends Row("bottom")
  override def toString = desc
}
object Row {
  def fromBitFlag(bitFlags: Int) = {
    Row.values.filter(att => (bitFlags & (1 << att.ordinal)) != 0).toList
  }
}
