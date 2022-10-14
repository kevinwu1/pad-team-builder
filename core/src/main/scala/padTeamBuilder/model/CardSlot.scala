package padTeamBuilder.model

enum CardSlot(desc: String) extends Enum[CardSlot] {
  case ThisCard extends CardSlot("this card")
  case YourLead extends CardSlot("your leader")
  case FriendLead extends CardSlot("friend leader")
  case AllSubs extends CardSlot("all subs")
  override def toString = desc
}

object CardSlot {
  def fromBitFlag(bits: Int): List[CardSlot] = {
    CardSlot.values.filter(att => (bits & (1 << att.ordinal)) != 0).toList
  }
}
