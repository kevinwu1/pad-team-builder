package skills.effects

import model.JsonSkillData
import model.Attribute
import model.JsonCardData

case class IncreaseSkyfall(
    colors: List[Attribute],
    skyfallIncreasePercent: Int,
    numTurns: Int
) extends SkillEffect {
  override def toString() = {
    val colorStr = colors.mkString(", ")
    s"For $numTurns turns, $skyfallIncreasePercent% increased skyfall for $colorStr. "
  }
}

object IncreaseSkyfall extends SkillEffectParser {

  override def parse(
      args: List[Int],
      skillData: Array[JsonSkillData],
      cardData: Array[JsonCardData]
  ): List[SkillEffect] = {
    List(
      IncreaseSkyfall(
        colors = Attribute.fromBitFlag(args(0)),
        skyfallIncreasePercent = args(3),
        numTurns = args(1)
      )
    )
  }

}
