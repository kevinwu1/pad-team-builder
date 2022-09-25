package skills.effects

import model.JsonSkillData
import model.Attribute
import model.JsonCardData

case class Shield(
    reductionPercent: Int,
    numTurns: Int
) extends SkillEffect {
  override def toString() = {
    s"Reduces damage taken by ${reductionPercent}% for $numTurns turns."
  }
}

object Shield extends SkillEffectParser {

  override def parse(
      args: List[Int],
      skillData: Array[JsonSkillData],
      cardData: Array[JsonCardData]
  ): List[SkillEffect] = {
    List(
      Shield(args(1), args(0))
    )
  }

}
