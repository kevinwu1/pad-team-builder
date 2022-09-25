package skills.effects

import model.JsonSkillData
import model.Attribute
import model.JsonCardData

case class Transform(
    targetId: Int,
    targetName: String
) extends SkillEffect {
  override def toString() = {
    s"Transform into #$targetId - $targetName"
  }
}

object Transform extends SkillEffectParser {
  override def parse(
      args: List[Int],
      skillData: Array[JsonSkillData],
      cardData: Array[JsonCardData]
  ): List[SkillEffect] = {

    val cardId = args(0)
    val card = cardData(cardId)
    List(Transform(targetId = cardId, card.name))
  }

}
