package skills.effects

import model.JsonSkillData
import model.Attribute
import model.JsonCardData

case class DefenseBreak(
    percent: Int,
    turns: Int
) extends SkillEffect {
  override def toString() = {
    s"Reduce enemy defense by ${percent}% for $turns turns."
  }
}

object DefenseBreak extends SkillEffectParser {

  override def parse(
      args: List[Int],
      skillData: Array[JsonSkillData],
      cardData: Array[JsonCardData]
  ): List[SkillEffect] = {
    List(
      DefenseBreak(args(1), args(0))
    )
  }

}
