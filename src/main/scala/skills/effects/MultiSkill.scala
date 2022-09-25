package skills.effects

import skills.ActiveSkill
import model.JsonSkillData
import model.JsonCardData

object MultiSkill extends SkillEffectParser {
  def parse(
      args: List[Int],
      skillData: Array[JsonSkillData],
      cardData: Array[JsonCardData]
  ): List[SkillEffect] = {
    args.flatMap(jsdId =>
      ActiveSkill.effectsFromJson(skillData(jsdId), skillData, cardData)
    )
  }
}
