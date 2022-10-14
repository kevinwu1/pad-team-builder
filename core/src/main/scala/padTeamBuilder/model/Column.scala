package padTeamBuilder.model

enum Column(desc: String) extends Enum[Column] {
  case L extends Column("leftmost")
  case L2 extends Column("2nd from the left")
  case L3 extends Column("3rd from the left")
  case R3 extends Column("3rd from the right")
  case R2 extends Column("2nd from the right")
  case R extends Column("rightmost")
  override def toString = desc
}
object Column {
  def fromBitFlag(bitFlags: Int) = {
    Column.values.filter(att => (bitFlags & (1 << att.ordinal)) != 0).toList
  }
}
