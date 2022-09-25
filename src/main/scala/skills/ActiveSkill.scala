package skills

import model.JsonSkillData
import skills.effects._
import model.Card
import model.JsonCardData

case class ActiveSkill(
    name: String,
    skillEffects: List[SkillEffect],
    cd: Int,
    maxLevel: Int
) {
  val maxLevelCd = cd - (maxLevel - 1)
  override def toString() = {
    skillEffects.mkString("\n")
  }
}

object ActiveSkill {
  def fromJson(
      jsonId: Int,
      skillData: Array[JsonSkillData],
      cardData: Array[JsonCardData]
  ): ActiveSkill = {
    val jsd = skillData(jsonId)
    val skillEffects = effectsFromJson(jsd, skillData, cardData)
    ActiveSkill(
      name = jsd.name,
      skillEffects = skillEffects,
      cd = jsd.initialCooldown.toInt,
      maxLevel = jsd.maxLevel.toInt
    )
  }

  def effectsFromJson(
      jsd: JsonSkillData,
      skillData: Array[JsonSkillData],
      cardData: Array[JsonCardData]
  ): List[SkillEffect] = {
    val parser: SkillEffectParser = jsd.internalEffectId match {
      case 0   => DamageElementalAoeScaling
      case 1   => DamageElementalAoeFixed
      case 2   => DamageSingleScaling
      case 3   => Shield
      case 4   => Poison
      case 5   => ChangeTheWorld
      case 6   => Gravity
      case 7   => HealScaling
      case 8   => HealFlat
      case 9   => ChangeXtoY
      case 10  => Refresh
      case 18  => Delay
      case 19  => DefenseBreak
      case 116 => MultiSkill
      case 126 => IncreaseSkyfall
      case 202 => Transform
    }
    parser.parse(jsd.args, skillData, cardData)
  }
}
