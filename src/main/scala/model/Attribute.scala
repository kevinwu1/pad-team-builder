package model

enum Attribute extends Enum[Attribute] {
  case FIRE, WATER, WOOD, LIGHT, DARK, HEART, JAMMER, POISON, MORTALPOISON, BOMB

  override def toString() = {
    if (this == MORTALPOISON)
      "Mortal Poison"
    else
      this.name.toLowerCase.capitalize
  }
}

object Attribute {
  def fromBitFlag(i: Int): List[Attribute] = {
    Attribute.values.filter(att => (i & (1 << att.ordinal)) != 0).toList
  }

  def firstFromBitFlag(bitFlag: Int): Attribute = {
    Attribute.values.find(att => ((1 << att.ordinal()) & bitFlag) != 0).get
  }

  def from(value: Long): Attribute = {
    Attribute.fromOrdinal(value.toInt)
  }
  def from(value: Int): Attribute = {
    Attribute.fromOrdinal(value)
  }
}
