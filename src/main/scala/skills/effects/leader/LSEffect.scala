package skills.effects.leader

import model._

//is this a monoid?
trait LSEffect(str: String) {
  override def toString = str
  def and(other: LSEffect): LSEffect = {
    (this, other) match {
      case (_, LSEffectNone) => this
      case (LSEffectNone, _) => other
      case (MultiLSEffect(e1), MultiLSEffect(e2)) =>
        MultiLSEffect(e1 ::: e2)
      case (MultiLSEffect(effectsList), e2) =>
        MultiLSEffect(effectsList :+ e2)
      case (e1, MultiLSEffect(effectsList2)) =>
        MultiLSEffect(e1 +: effectsList2)
      case _ =>
        MultiLSEffect(List(this, other))
    }
  }
}

case class LSPassive(payoff: LSPayoff)
    extends LSEffect(payoff.toString.capitalize)

// component is condition + payoff
case class LSComponent(condition: LSCondition, payoff: LSPayoff)
    extends LSEffect(
      s"If $condition, then $payoff"
    )
case class LSComponentInfinite(condition: LSCondition, payoff: LSPayoff)
    extends LSEffect(
      s"For each $condition, $payoff"
    )

case class MultiLSEffect(effects: List[LSEffect])
    extends LSEffect(
      effects.mkString("\n")
    )

object LSEffectNone extends LSEffect("")

def firstOf(effects: List[LSEffect]): LSEffect = {
  if (effects.size == 1)
    effects.head
  else
    DynamicSelectEffect(effects)
}

case class DynamicSelectEffect(effects: List[LSEffect])
    extends LSEffect(
      s"Up to 1 of the following effects: [\n${effects.zipWithIndex
          .map((e, i) => s"${i + 1}. $e")
          .mkString("\n")}\n]"
    )
