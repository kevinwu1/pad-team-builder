package skills.effects

import model.JsonSkillData
import model.Attribute
import model.JsonCardData

case class Refresh(
) extends SkillEffect {
  override def toString() = {
    s"Replaces all orbs."
  }
}

object Refresh extends SkillEffectParser {

  override def parse(
      args: List[Int],
      skillData: Array[JsonSkillData],
      cardData: Array[JsonCardData]
  ): List[SkillEffect] = {
    List(
      Refresh()
    )
  }

}
