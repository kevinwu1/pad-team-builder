package skills.effects

import model.JsonSkillData
import model.Attribute
import model.JsonCardData

case class OrbChangeABtoCD(
    from1: Attribute,
    to1: Attribute,
    from2: Attribute,
    to2: Attribute
) extends SkillEffect {
  override def toString() = {
    if (to1 == to2)
      s"Changes $from1 and $from2 orbs to $to1."
    else
      s"Changes $from1 orbs to $to1 and $from2 orbs to $to2."

  }
}

object OrbChangeABtoCD extends SkillEffectParser {

  override def parse(
      args: List[Int],
      skillData: Array[JsonSkillData],
      cardData: Array[JsonCardData]
  ): List[SkillEffect] = {
    List(
      OrbChangeABtoCD(
        Attribute.from(args(0)),
        Attribute.from(args(1)),
        Attribute.from(args(2)),
        Attribute.from(args(3))
      )
    )
  }

}
