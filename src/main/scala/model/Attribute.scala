package model

enum Attribute extends Enum[Attribute] {
  case FIRE, WATER, WOOD, LIGHT, DARK, HEART, JAMMER, POISON, MORTALPOISON,
    BOMB, NONE

  override def toString() = {
    if (this == MORTALPOISON)
      "Mortal Poison"
    else
      this.name.toLowerCase.capitalize
  }
}

object Attribute {
  def fromBitFlag(bits: Int): List[Attribute] = {
    Attribute.values.filter(att => (bits & (1 << att.ordinal)) != 0).toList
  }

  def firstFromBitFlag(bitFlag: Int): Attribute = {
    Attribute.values.find(att => ((1 << att.ordinal()) & bitFlag) != 0).get
  }

  def from(value: Long): Attribute = from(value.toInt)

  def from(value: Int): Attribute = {
    if (value == -1)
      NONE
    else
      Attribute.fromOrdinal(value)
  }
}
