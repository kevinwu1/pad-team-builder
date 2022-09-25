package skills.effects

import model.JsonSkillData
import model.Attribute
import model.JsonCardData

case class ChangeTheWorld(
    seconds: Int
) extends SkillEffect {
  override def toString() = {
    s"Move orbs freely for ${seconds} seconds."
  }
}

object ChangeTheWorld extends SkillEffectParser {

  override def parse(
      args: List[Int],
      skillData: Array[JsonSkillData],
      cardData: Array[JsonCardData]
  ): List[SkillEffect] = {
    List(
      ChangeTheWorld(args(0))
    )
  }

}
