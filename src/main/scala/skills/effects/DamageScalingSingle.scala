package skills.effects

import model.JsonSkillData
import model.Attribute
import model.JsonCardData

case class DamageScalingSingle(
    multiplier: Int
) extends SkillEffect {
  override def toString() = {
    s"Inflicts a ${multiplier}x attack to 1 enemy."
  }
}

object DamageScalingSingle extends SkillEffectParser {
  override def parse(
      args: List[Int],
      skillData: Array[JsonSkillData],
      cardData: Array[JsonCardData]
  ): List[SkillEffect] = {
    List(
      DamageScalingSingle(args(0) / 100)
    )
  }

}
