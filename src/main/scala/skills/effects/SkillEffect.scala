package skills.effects

import model.JsonSkillData
import model.Card
import model.JsonCardData

trait SkillEffect {}

trait SkillEffectParser {
  def parse(
      args: List[Int],
      skillData: Array[JsonSkillData],
      cardData: Array[JsonCardData]
  ): List[SkillEffect]
}
