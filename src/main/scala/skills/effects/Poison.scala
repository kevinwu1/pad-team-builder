package skills.effects

import model.JsonSkillData
import model.Attribute
import model.JsonCardData

case class Poison(
    multiplier: Int
) extends SkillEffect {
  override def toString() = {
    s"Poisons enemies with ${multiplier}x ATK."
  }
}

object Poison extends SkillEffectParser {

  override def parse(
      args: List[Int],
      skillData: Array[JsonSkillData],
      cardData: Array[JsonCardData]
  ): List[SkillEffect] = {
    List(
      Poison(args(0) / 100)
    )
  }

}
