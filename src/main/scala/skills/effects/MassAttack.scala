package skills.effects

import model.JsonSkillData
import model.Attribute
import model.JsonCardData

case class MassAttack(
    numTurns: Int
) extends SkillEffect {
  override def toString() = {
    s"Mass attacks for $numTurns turns."
  }
}

object MassAttack extends SkillEffectParser {
  override def parse(
      args: List[Int],
      skillData: Array[JsonSkillData],
      cardData: Array[JsonCardData]
  ): List[SkillEffect] = {
    List(
      MassAttack(args(0))
    )
  }

}
