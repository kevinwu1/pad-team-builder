package skills.effects

import model.JsonSkillData
import model.Attribute
import model.JsonCardData

case class HealFlat(
    multiplier: Int
) extends SkillEffect {
  override def toString() = {
    s"Heal ${multiplier} HP."
  }
}

object HealFlat extends SkillEffectParser {
  override def parse(
      args: List[Int],
      skillData: Array[JsonSkillData],
      cardData: Array[JsonCardData]
  ): List[SkillEffect] = {
    List(
      HealFlat(args(0))
    )
  }

}
