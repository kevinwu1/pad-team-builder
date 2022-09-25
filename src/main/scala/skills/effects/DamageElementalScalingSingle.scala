package skills.effects

import model.JsonSkillData
import model.Attribute
import model.JsonCardData

case class DamageElementalScalingSingle(
    multiplier: Int,
    attribute: Attribute
) extends SkillEffect {
  override def toString() = {
    s"Inflicts a ${multiplier}x $attribute attack on 1 enemy."
  }
}

object DamageElementalScalingSingle extends SkillEffectParser {
  override def parse(
      args: List[Int],
      skillData: Array[JsonSkillData],
      cardData: Array[JsonCardData]
  ): List[SkillEffect] = {
    List(
      DamageElementalScalingSingle(args(1) / 100, Attribute.from(args(0)))
    )
  }

}
