package skills.effects.leader

import model._

//is this a monoid?
sealed trait LSCondition(str: String) {
  override def toString = str
  def and(other: LSCondition): LSCondition = {
    (this, other) match {
      case (_, LSConditionNone) => this
      case (LSConditionMulti(e1), LSConditionMulti(e2)) =>
        LSConditionMulti(e1 ::: e2)
      case (LSConditionMulti(effectsList), e2) =>
        LSConditionMulti(effectsList :+ e2)
      case (e1, LSConditionMulti(effectsList2)) =>
        LSConditionMulti(e1 +: effectsList2)
      case _ =>
        LSConditionMulti(List(this, other))
    }
  }
}

object LSConditionNone extends LSCondition("no condition")

case class LSConditionMulti(conditions: List[LSCondition])
    extends LSCondition(
      conditions.mkString(" and ")
    )

case class ConditionAttribute(att: Attribute)
    extends LSCondition(
      s"$att attribute"
    )
case class ConditionType(typ: CardType)
    extends LSCondition(
      s"$typ type"
    )

sealed trait ConditionMatchingOrbs
object ConditionMatchingAnyOrbs
    extends ConditionMatchingOrbs
    with LSCondition(
      s"any orbs matched"
    )

case class ConditionHPThreshold(
    hpPercent: Int,
    isGreater: Boolean,
    allowEqual: Boolean
) extends LSCondition(
      s"HP ${if (isGreater) ">" else "<"}${if (allowEqual) "=" else ""} $hpPercent%"
    )

case class ConditionColorsMatched(atts: List[Attribute], matchRequirement: Int)
    extends LSCondition(
      s"$matchRequirement of ${atts.mkString(", ")} matched"
    )

case class ConditionCombos(numCombos: Int)
    extends LSCondition(
      s"$numCombos combos made"
    )
case class ConditionCombosExact(numCombos: Int)
    extends LSCondition(
      s"exactly $numCombos combos made"
    )

case class ConditionOrbsLinked(numOrbs: Int, att: Attribute)
    extends LSCondition(
      s"$numOrbs linked $att orbs"
    )

case class ConditionCardOnTeam(cardId: Int)
    extends LSCondition(
      s"$cardId is on the team"
    )

object SkillUsed
    extends LSCondition(
      s"a skill was used"
    )
