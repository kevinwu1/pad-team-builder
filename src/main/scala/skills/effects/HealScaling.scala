package skills.effects

import model.JsonSkillData
import model.Attribute
import model.JsonCardData

case class HealScaling(
    multiplier: Int
) extends SkillEffect {
  override def toString() = {
    s"Heal ${multiplier}x this card's RCV."
  }
}

object HealScaling extends SkillEffectParser {
  override def parse(
      args: List[Int],
      skillData: Array[JsonSkillData],
      cardData: Array[JsonCardData]
  ): List[SkillEffect] = {
    List(
      HealScaling(args(0) / 100)
    )
  }

}
