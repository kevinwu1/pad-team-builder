package skills.effects

import model.JsonSkillData
import model.Attribute
import model.JsonCardData

case class SpikeAttribute(
    multiplier: Double,
    att: Attribute,
    numTurns: Int
) extends SkillEffect {
  override def toString() = {
    if (att == Attribute.HEART)
      s"${multiplier}x RCV for $numTurns turns."
    else
      s"${multiplier}x $att ATK for $numTurns turns."
  }
}

object SpikeAttribute extends SkillEffectParser {
  override def parse(
      args: List[Int],
      skillData: Array[JsonSkillData],
      cardData: Array[JsonCardData]
  ): List[SkillEffect] = {
    List(
      SpikeAttribute(args(2) / 100.0, Attribute.from(args(1)), args(0))
    )
  }

}
