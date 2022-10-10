package padTeamBuilder.model

enum CardType {
  case Evo,
    Balanced,
    Physical,
    Healer,
    Dragon,
    God,
    Attacker,
    Devil,
    Machine,
    UNKNOWN9,
    UNKNOWN10,
    UNKNOWN11,
    Awakening,
    UNKNOWN13,
    Enhance,
    Redeemable,
    NoType
}

object CardType {
  def from(cardType: Int): CardType = {
    cardType match {
      case -1 => CardType.NoType
      case x  => CardType.fromOrdinal(x.toInt)
    }
  }

  def fromBitFlag(bits: Int): List[CardType] = {
    CardType.values.filter(att => (bits & (1 << att.ordinal)) != 0).toList
  }

  def firstFromBitFlag(bits: Int): CardType = {
    val types = fromBitFlag(bits)
    if (types.size == 1)
      types.head
    else
      ???
  }
}
