package skills.effects

import model.JsonSkillData
import model.Attribute
import model.JsonCardData

case class ChangeXtoY(
    from: Attribute,
    to: Attribute
) extends SkillEffect {
  override def toString() = {
    s"Changes $from orbs to $to orbs."
  }
}

object ChangeXtoY extends SkillEffectParser {

  override def parse(
      args: List[Int],
      skillData: Array[JsonSkillData],
      cardData: Array[JsonCardData]
  ): List[SkillEffect] = {
    List(
      ChangeXtoY(Attribute.from(args(0)), Attribute.from(args(1)))
    )
  }

}
