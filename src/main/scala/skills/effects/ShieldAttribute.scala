package skills.effects

import model.JsonSkillData
import model.Attribute
import model.JsonCardData

case class ShieldAttribute(
    numTurns: Int,
    attribute: Attribute,
    reductionPercent: Int
) extends SkillEffect {
  override def toString() = {
    s"For $numTurns turns, ${reductionPercent}% reduced $attribute damage taken"
  }
}

object ShieldAttribute extends SkillEffectParser {

  override def parse(
      args: List[Int],
      skillData: Array[JsonSkillData],
      cardData: Array[JsonCardData]
  ): List[SkillEffect] = {
    List(
      ShieldAttribute(args(0), Attribute.from(args(1)), args(2))
    )
  }

}
