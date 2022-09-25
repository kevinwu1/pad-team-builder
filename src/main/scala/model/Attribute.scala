package model

enum Attribute extends Enum[Attribute] {
  case FIRE, WATER, WOOD, LIGHT, DARK, HEART

  override def toString() = {
    this.name.toLowerCase.capitalize
  }
}

object Attribute {
  def fromBitFlag(i: Int): List[Attribute] = {
    Attribute.values.filter(att => (i & (1 << att.ordinal)) != 0).toList
  }

  def from(value: Long): Attribute = {
    Attribute.fromOrdinal(value.toInt)
  }
}
