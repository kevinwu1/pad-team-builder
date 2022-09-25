package skills.effects

import model.JsonSkillData
import model.Attribute
import model.JsonCardData

case class Gravity(
    percent: Int
) extends SkillEffect {
  override def toString() = {
    s"Reduce all enemies' current HP by ${percent}%."
  }
}

object Gravity extends SkillEffectParser {
  override def parse(
      args: List[Int],
      skillData: Array[JsonSkillData],
      cardData: Array[JsonCardData]
  ): List[SkillEffect] = {
    List(
      Gravity(args(0))
    )
  }

}
