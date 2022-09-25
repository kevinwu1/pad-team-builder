package skills.effects

import model.JsonSkillData
import model.Attribute
import model.JsonCardData

case class DrainSingleScaling(
    multiplier: Int,
    healPercent: Int
) extends SkillEffect {
  override def toString() = {
    s"Inflicts a ${multiplier}x attack to 1 enemy and heal ${healPercent}% of the damage."
  }
}

object DrainSingleScaling extends SkillEffectParser {
  override def parse(
      args: List[Int],
      skillData: Array[JsonSkillData],
      cardData: Array[JsonCardData]
  ): List[SkillEffect] = {
    List(
      DrainSingleScaling(args(0) / 100, args(1))
    )
  }

}
