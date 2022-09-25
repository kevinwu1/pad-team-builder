package skills.effects

import model.JsonSkillData
import model.Attribute
import model.JsonCardData

case class DamageElementalFixedToAttribute(
    damage: Int,
    damageAttribute: Attribute,
    targetAttribute: Attribute
) extends SkillEffect {
  override def toString() = {
    s"Inflict $damage $damageAttribute damage to $targetAttribute enemies."
  }
}

object DamageElementalFixedToAttribute extends SkillEffectParser {
  override def parse(
      args: List[Int],
      skillData: Array[JsonSkillData],
      cardData: Array[JsonCardData]
  ): List[SkillEffect] = {
    List(
      DamageElementalFixedToAttribute(
        args(2),
        Attribute.from(args(1)),
        Attribute.from(args(0))
      )
    )
  }

}
