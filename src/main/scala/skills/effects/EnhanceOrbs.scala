package skills.effects

import model.JsonSkillData
import model.Attribute
import model.JsonCardData

case class EnhanceOrbs(
    attribute: Attribute
) extends SkillEffect {
  override def toString() = {
    s"Enhances $attribute orbs."
  }
}

object EnhanceOrbs extends SkillEffectParser {
  override def parse(
      args: List[Int],
      skillData: Array[JsonSkillData],
      cardData: Array[JsonCardData]
  ): List[SkillEffect] = {
    List(
      EnhanceOrbs(Attribute.from(args(0)))
    )
  }

}
