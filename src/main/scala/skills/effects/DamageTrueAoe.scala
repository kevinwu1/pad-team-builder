package skills.effects

import model.JsonSkillData
import model.Attribute
import model.JsonCardData

case class DamageTrueAoe(
    damage: Int
) extends SkillEffect {
  override def toString() = {
    s"Inflict $damage True damage to all enemies."
  }
}

object DamageTrueAoe extends SkillEffectParser {
  override def parse(
      args: List[Int],
      skillData: Array[JsonSkillData],
      cardData: Array[JsonCardData]
  ): List[SkillEffect] = {
    List(
      DamageTrueAoe(args(0))
    )
  }

}
