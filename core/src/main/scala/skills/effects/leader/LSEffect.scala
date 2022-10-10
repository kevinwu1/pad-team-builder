package padTeamBuilder.skills.effects.leader

import padTeamBuilder.model._

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
      s"${payoff.toString.capitalize} if $condition. "
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
  val effectsFiltered = effects.filter(_ != LSEffectNone)
  if (effectsFiltered.size == 0)
    LSEffectNone
  else if (effectsFiltered.size == 1)
    effectsFiltered.head
  else
    DynamicSelectEffect(effectsFiltered)
}

case class DynamicSelectEffect(effects: List[LSEffect])
    extends LSEffect(
      s"Up to 1 of the following effects: [\n${effects.zipWithIndex
          .map((e, i) => s"${i + 1}. $e")
          .mkString("\n")}\n]"
    )

case class LSAttOrTypeScaling(
    atts: List[Attribute],
    types: List[CardType],
    hpScaling: Double,
    atkScaling: Double,
    rcvScaling: Double
) extends LSEffect({
      val payoffs =
        List("hp", "atk", "rcv")
          .zip(List(hpScaling, atkScaling, rcvScaling))
          .filter(_._2 != 0)
      {
        if (types.size != 0)
          payoffs
            .map(t => s"${scalingHelper(t._2, types, "type")} ${t._1}. ")
            .mkString("\n")
        else ""
      }
        + {
          if (atts.size != 0)
            payoffs
              .map(t => s"${scalingHelper(t._2, atts, "atts")} ${t._1}. ")
              .mkString("\n")
          else ""
        }
    })

private def scalingHelper(
    scale: Double,
    types: List[CardType | Attribute],
    keyword: String
): String = {
  if (types.size == 0)
    ""
  else
    s"1+($scale*(${types.map(t => s"# of $t $keyword").mkString(" + ")}))x"
}
