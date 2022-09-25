package skills.effects

import model.JsonSkillData
import model.Attribute
import model.JsonCardData

case class DamageElementalRangeSingle(
    floor: Int,
    ceil: Int,
    attribute: Attribute
) extends SkillEffect {
  override def toString() = {
    if (floor == ceil)
      s"Inflicts a ${floor}x $attribute attack on 1 enemies."
    else
      s"Inflicts a ${floor}x-${ceil}x $attribute attack on 1 enemies."
  }
}

object DamageElementalRangeSingle extends SkillEffectParser {
  override def parse(
      args: List[Int],
      skillData: Array[JsonSkillData],
      cardData: Array[JsonCardData]
  ): List[SkillEffect] = {
    List(
      DamageElementalRangeSingle(
        args(1) / 100,
        args(2) / 100,
        Attribute.fromOrdinal(args(0))
      )
    )
  }

}
