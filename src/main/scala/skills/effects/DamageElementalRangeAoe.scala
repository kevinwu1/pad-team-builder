package skills.effects

import model.JsonSkillData
import model.Attribute
import model.JsonCardData

case class DamageElementalRangeAoe(
    floor: Int,
    ceil: Int,
    attribute: Attribute
) extends SkillEffect {
  override def toString() = {
    s"Inflicts a ${floor}x-${ceil}x $attribute attack on all enemies."
  }
}

object DamageElementalRangeAoe extends SkillEffectParser {
  override def parse(
      args: List[Int],
      skillData: Array[JsonSkillData],
      cardData: Array[JsonCardData]
  ): List[SkillEffect] = {
    List(
      DamageElementalRangeAoe(
        args(1) / 100,
        args(2) / 100,
        Attribute.fromOrdinal(args(0))
      )
    )
  }

}
