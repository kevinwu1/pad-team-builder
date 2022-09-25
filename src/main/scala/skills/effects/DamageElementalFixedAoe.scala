package skills.effects

import model.JsonSkillData
import model.Attribute
import model.JsonCardData

case class DamageElementalFixedAoe(
    damageAmount: Int,
    attribute: Attribute
) extends SkillEffect {
  override def toString() = {
    s"Inflicts ${damageAmount} ${attribute} attack on all enemies."
  }
}

object DamageElementalFixedAoe extends SkillEffectParser {
  override def parse(
      args: List[Int],
      skillData: Array[JsonSkillData],
      cardData: Array[JsonCardData]
  ): List[SkillEffect] = {
    List(
      DamageElementalFixedAoe(args(1), Attribute.fromOrdinal(args(0)))
    )
  }

}
