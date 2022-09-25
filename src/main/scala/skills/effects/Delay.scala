package skills.effects

import model.JsonSkillData
import model.Attribute
import model.JsonCardData

case class Delay(
    turns: Int
) extends SkillEffect {
  override def toString() = {
    s"Delays ${turns} turns to all enemies."
  }
}

object Delay extends SkillEffectParser {
  override def parse(
      args: List[Int],
      skillData: Array[JsonSkillData],
      cardData: Array[JsonCardData]
  ): List[SkillEffect] = {
    List(
      Delay(args(0))
    )
  }

}
