package skills.effects

import model.JsonSkillData
import model.Attribute
import model.JsonCardData

case class DamageElementalAoeScaling(
    multiplier: Int,
    attribute: Attribute
) extends SkillEffect {
  override def toString() = {
    s"Inflicts a ${multiplier}x ${attribute} attack on all enemies."
  }
}

object DamageElementalAoeScaling extends SkillEffectParser {

  override def parse(
      args: List[Int],
      skillData: Array[JsonSkillData],
      cardData: Array[JsonCardData]
  ): List[SkillEffect] = {
    List(
      DamageElementalAoeScaling(args(1) / 100, Attribute.fromOrdinal(args(0)))
    )
  }

}
