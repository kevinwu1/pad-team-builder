package model

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
  def from(cardType: Long): CardType = {
    cardType match {
      case -1 => CardType.NoType
      case x  => CardType.fromOrdinal(x.toInt)
    }
  }
}
