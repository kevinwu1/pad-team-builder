package padTeamBuilder.model

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

  def firstFromBitFlag(bits: Int): Attribute = {
    val atts = fromBitFlag(bits)
    if (atts.size != 1)
      ???
    else
      atts.head
  }

  def from(value: Long): Attribute = from(value.toInt)

  def from(value: Int): Attribute = {
    if (value == -1)
      NONE
    else
      Attribute.fromOrdinal(value)
  }

  def fwwld = List(FIRE, WATER, WOOD, LIGHT, DARK)
}
