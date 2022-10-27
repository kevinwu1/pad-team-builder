package padTeamBuilder.skills.effects.leader

import padTeamBuilder.model._

//is this a monoid?
sealed trait LSEffect(str: String) {
  override def toString = str
  def and(other: LSEffect): LSEffect = {
    (this, other) match {
      case (_, LSEffectNone()) => this
      case (LSEffectNone(), _) => other
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

case class LSEffectNone() extends LSEffect("")

def firstOf(effects: List[LSEffect]): LSEffect = {
  val effectsFiltered = effects.filter(_ != LSEffectNone())
  if (effectsFiltered.size == 0)
    LSEffectNone()
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

sealed trait LSCondition(str: String) {
  override def toString = str
  def and(other: LSCondition): LSCondition = {
    (this, other) match {
      case (_, LSConditionNone()) => this
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

case class LSConditionNone() extends LSCondition("no condition")

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
case class ConditionMatchingAnyOrbs()
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
      s"at least $numCombos combos made"
    )
case class ConditionCombosExact(numCombos: Int)
    extends LSCondition(
      s"exactly $numCombos combos made"
    )

case class ConditionOrbsLinked(numOrbs: Int, att: Attribute)
    extends LSCondition(
      s"at least $numOrbs linked $att orbs"
    )

case class ConditionOrbsLinkedExact(numOrbs: Int, att: Attribute)
    extends LSCondition(
      s"exactly $numOrbs linked $att orbs"
    )

case class ConditionCardOnTeam(cardId: Int)
    extends LSCondition(
      s"$cardId is on the team"
    )

case class ConditionSkillUsed()
    extends LSCondition(
      s"a skill was used"
    )

case class ConditionSparkle()
    extends LSCondition(
      s"5 orbs with 1+ enhanced matched"
    )

case class ConditionCross(att: Attribute)
    extends LSCondition(
      s"$att cross matched"
    )

case class ConditionCollab(collab: Collab)
    extends LSCondition(
      s"all subs are from $collab collab"
    )

case class ConditionOrbsRemaining(numOrbs: Int)
    extends LSCondition(
      s"$numOrbs orbs remaining"
    )

case class ConditionLMatched(att: Attribute)
    extends LSCondition(
      s"$att L matched"
    )

case class ConditionCoop()
    extends LSCondition(
      s"in multiplayer mode"
    )

case class Healed(amt: Int)
    extends LSCondition(
      s"$amt healed"
    )

case class ConditionAllPixel()
    extends LSCondition(
      s"all subs are pixel cards"
    )

case class ConditionAllRevo()
    extends LSCondition(
      s"all subs are reincarnated cards"
    )

case class ConditionTeamRarity(maxRarity: Int)
    extends LSCondition(
      s"total team rarity is maxRarity or less"
    )

sealed trait LSPayoff(str: String) {
  override def toString = str
}

case class LSPayoffNone() extends LSPayoff("no effect")
sealed trait StatBoost
case class HPBoost(mult: Double)
    extends StatBoost
    with LSPayoff(
      s"${mult}x HP"
    )
case class ATKBoost(mult: Double)
    extends StatBoost
    with LSPayoff(
      s"${mult}x ATK"
    )
case class RCVBoost(mult: Double)
    extends StatBoost
    with LSPayoff(
      s"${mult}x RCV"
    )

sealed trait Shield

case class ShieldElement(reduction: Int, att: Attribute)
    extends Shield
    with LSPayoff(
      s"$reduction% reduced $att damage taken"
    )

case class ShieldRegular(reduction: Int)
    extends Shield
    with LSPayoff(
      s"$reduction% reduced damage taken"
    )

sealed trait BonusAttack
case class BonusAttackScaling(ratio: Double)
    extends BonusAttack
    with LSPayoff(
      s"inflicts a bonus attack equal to ${ratio}x ATK"
    )

case class BonusAttackFixed(dmg: Int)
    extends BonusAttack
    with LSPayoff(
      s"inflicts a $dmg damage bonus attack"
    )

trait AutoHeal
case class AutoHealScaling(ratio: Double)
    extends AutoHeal
    with LSPayoff(
      s"heal HP equal to ${ratio}x RCV"
    )

case class Resolve()
    extends LSPayoff(
      s"survive a single hit"
    )

case class PayoffChance(chance: Int, payoff: LSPayoff)
    extends LSPayoff(
      s"$chance% chance for $payoff"
    )

def Chance(chance: Int, payoff: LSPayoff): LSPayoff = {
  if (chance != 100)
    PayoffChance(chance, payoff)
  else payoff
}

case class DrummingSound()
    extends LSPayoff(
      s"A drumming sound is made when orbs are moved"
    )

case class DropRateBoost(rate: Double)
    extends LSPayoff(
      s"${rate}x coin drop rate when entering a dungeon as Leader"
    )

case class CoinBoost(rate: Double)
    extends LSPayoff(
      s"${rate}x coin drop rate when entering a dungeon as Leader"
    )

case class CounterAttack(mult: Double, att: Attribute)
    extends LSPayoff(
      s"${mult}x $att counterattack"
    )

case class TimeExtend(seconds: Double)
    extends LSPayoff(
      s"${if (seconds > 0) "+" else ""}$seconds seconds move time"
    )

case class RankExpBoost(rate: Double)
    extends LSPayoff(
      s"${rate}x rank EXP"
    )

case class NoSkyfall()
    extends LSPayoff(
      s"No skyfall"
    )

case class Board7x6()
    extends LSPayoff(
      s"7x6 board"
    )

case class AddCombos(numCombos: Int)
    extends LSPayoff(
      s"adds $numCombos combo${if (numCombos == 1) "" else "s"}"
    )

case class FixedTime(seconds: Double)
    extends LSPayoff(
      s"fixes move time at $seconds seconds"
    )

case class VoidPoisonDamage()
    extends LSPayoff(
      s"voids poison damage"
    )

case class MinimumMatch(min: Int)
    extends LSPayoff(
      s"cannot clear matches of ${min - 1} or fewer orbs"
    )

case class PayoffAwokenBindClear(turns: Int)
    extends LSPayoff(
      s"awoken bind reduce by $turns turns"
    )

case class AddAwakening(awk: Awakening)
    extends LSPayoff(
      s"adds $awk awakening"
    )
