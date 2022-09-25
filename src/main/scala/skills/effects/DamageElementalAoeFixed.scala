package skills.effects

import model.JsonSkillData
import model.Attribute
import model.JsonCardData

case class DamageElementalAoeFixed(
    damageAmount: Int,
    attribute: Attribute
) extends SkillEffect {
  override def toString() = {
    s"Inflicts ${damageAmount} ${attribute} attack on all enemies."
  }
}

object DamageElementalAoeFixed extends SkillEffectParser {

  override def parse(
      args: List[Int],
      skillData: Array[JsonSkillData],
      cardData: Array[JsonCardData]
  ): List[SkillEffect] = {
    List(
      DamageElementalAoeFixed(args(1), Attribute.fromOrdinal(args(0)))
    )
  }

}
