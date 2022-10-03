package skills.effects.leader

import model._

sealed trait LSCondition(str: String) {
  override def toString = str
}

object LSConditionNone extends LSCondition("no condition")

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
