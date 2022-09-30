package skills.effects

import model.JsonSkillData
import model.Card
import model.JsonCardData
import model.Attribute
import model.CardType
import skills.ActiveSkill
import model.Awakening

trait SkillEffect(str: String) {
  def and(other: SkillEffect): SkillEffect = {
    (this, other) match {
      case (_, NoEffect) => this
      case (MultiEffect(e1), MultiEffect(e2)) =>
        MultiEffect(e1 ::: e2)
      case (MultiEffect(effectsList), effect2) =>
        MultiEffect(effectsList :+ effect2)
      case (effect1, MultiEffect(effectsList2)) =>
        MultiEffect(effectsList2 :+ effect1)
      case nonMultiEffect =>
        MultiEffect(List(this, other))
    }
  }

  override def toString = str
}

object NoEffect extends SkillEffect("") {
  override def and(other: SkillEffect) = other
}

case class MultiEffect(effects: List[SkillEffect])
    extends SkillEffect(effects.mkString("\n"))

case class ChangeTheWorld(
    seconds: Int
) extends SkillEffect(s"Move orbs freely for ${seconds} seconds. ")

case class CounterAttack(
    multiplier: Int,
    att: Attribute,
    turns: Int
) extends SkillEffect(s"${multiplier}x $att counterattack for $turns turns. ")

case class Suicide(percentLost: Double)
    extends SkillEffect(s"HP reduced by ${percentLost}%. ")

object FullSuicide extends SkillEffect(s"HP reduced to 1. ")

case class DefenseBreak(
    percent: Int,
    turns: Int
) extends SkillEffect(s"Reduce enemy defense by ${percent}% for $turns turns. ")

case class Delay(
    turns: Int
) extends SkillEffect(s"Delays ${turns} turns to all enemies. ")

case class EnhanceOrbs(
    attribute: Attribute
) extends SkillEffect(s"Enhances $attribute orbs. ")

sealed trait Gravity

case class GravityFalse(
    percent: Int
) extends SkillEffect(s"Reduce all enemies' current HP by ${percent}%. ")

sealed trait Heal

case class HealFlat(
    amount: Int
) extends SkillEffect(s"Heal ${amount} HP. ")

case class HealMultiplier(
    multiplier: Int
) extends SkillEffect(s"Heal ${multiplier}x this card's RCV. ")

case class HealPercentMax(
    percent: Int
) extends SkillEffect(s"Heal ${percent}% max HP. ")

case class HealScalingByAwakening(percent: Int, awks: List[Awakening])
    extends Heal
    with SkillEffect(
      s"Heal $percent% RCV as HP for each ${awks.mkString(", ")} awakening on the team. "
    )

case class IncreaseSkyfall(
    colors: List[Attribute],
    percent: Int,
    turns: Int
) extends SkillEffect(
      s"$percent% increased skyfall for ${colors.mkString(", ")} for $turns turns. "
    )

case class MassAttack(
    turns: Int
) extends SkillEffect(s"Mass attacks for $turns turns. ")

sealed trait OrbChange

case class OrbChangeAtoB(
    from: Attribute,
    to: Attribute
) extends SkillEffect(s"Changes $from orbs to $to orbs. ")
    with OrbChange

case class OrbChangeFullBoard(
    attribute: List[Attribute]
) extends SkillEffect(s"Change all orbs to ${attribute.mkString(", ")}. ")
    with OrbChange

enum Column(desc: String) {
  case L extends Column("leftmost")
  case L2 extends Column("2nd from the left")
  case L3 extends Column("3rd from the left")
  case R3 extends Column("3rd from the right")
  case R2 extends Column("2nd from the right")
  case R extends Column("rightmost")
  override def toString = desc
}
object Column {
  def fromBitFlag(bitFlags: Int) = {
    Column.values.filter(att => (bitFlags & (1 << att.ordinal)) != 0).toList
  }
}

case class OrbChangeColumn(
    column: Column,
    att: Attribute
) extends OrbChange
    with SkillEffect(s"Changes the $column column into $att. ")

enum Row(desc: String) {
  case T extends Row("top")
  case T2 extends Row("2nd from the top")
  case C extends Row("center")
  case B2 extends Row("2nd from the bottom")
  case B extends Row("bottom")
  override def toString = desc
}
object Row {
  def fromBitFlag(bitFlags: Int) = {
    Row.values.filter(att => (bitFlags & (1 << att.ordinal)) != 0).toList
  }
}

case class OrbChangeRow(
    row: Row,
    att: Attribute
) extends OrbChange
    with SkillEffect(s"Changes the $row row into $att. ")

case class OrbChangeRandomSpawn(
    numOrbs: Int,
    spawnAtts: List[Attribute],
    exclAtts: List[Attribute]
) extends OrbChange
    with SkillEffect(s"Randomly spawn $numOrbs ${spawnAtts
        .mkString(", ")} orbs from non ${exclAtts.mkString(", ")} orbs. ")

case class Poison(
    multiplier: Int
) extends SkillEffect(s"Poisons enemies with ${multiplier}x ATK. ")

object Refresh extends SkillEffect(s"Replaces all orbs. ")

sealed trait Shield
case class ShieldAll(
    percent: Int,
    turns: Int
) extends Shield
    with SkillEffect(s"Reduces damage taken by ${percent}% for $turns turns. ")

case class ShieldAttribute(
    turns: Int,
    attribute: Attribute,
    percent: Int
) extends Shield
    with SkillEffect(
      s"For $turns turns, ${percent}% reduced $attribute damage taken"
    )

case class ShieldScalingByAwakening(
    reductionScaling: Double,
    awks: List[Awakening],
    turns: Int
) extends Shield
    with SkillEffect(
      s"${reductionScaling}% damage reduction for each ${awks.mkString(", ")} awakening on the team for $turns turns. "
    )

trait Spike
case class SpikeAttribute(
    multiplier: Double,
    att: Attribute,
    turns: Int
) extends Spike
    with SkillEffect(
      if (att == Attribute.HEART)
        s"${multiplier}x RCV for $turns turns. "
      else
        s"${multiplier}x ATK for $att attribute for $turns turns. "
    )

case class SpikeType(
    multiplier: Double,
    cardType: CardType,
    turns: Int
) extends SkillEffect(
      s"${multiplier}x ATK for $cardType type for $turns turns. "
    )

case class SpikeScalingByAwakening(
    dmgScaling: Double,
    awks: List[Awakening],
    turns: Int
) extends Spike
    with SkillEffect(
      s"1+(${dmgScaling}x) ATK for each ${awks.mkString(", ")} awakening on the team for $turns turns. "
    )

case class Transform(
    targetId: Int,
    targetName: String
) extends SkillEffect(s"Transform into #$targetId - $targetName")

object LeadSwap
    extends SkillEffect(
      s"Switches places with leader. Switch back when used again. "
    )

case class AwokenBindClear(turns: Int)
    extends SkillEffect(s"$turns turn awoken bind clear. ")

case class BindClear(turns: Int)
    extends SkillEffect(s"$turns turn bind clear. ")

case class Random(effects: List[ActiveSkill])
    extends SkillEffect({
      val skills =
        effects.zipWithIndex.map((s, i) => s"${i + 1}. $s").mkString("\n")
      s"Randomly activates one of the following: \n$skills"
    })

trait TimeExtend

case class TimeExtendFlat(seconds: Int, turns: Int)
    extends TimeExtend
    with SkillEffect(
      s"${if (seconds > 0) "Extends" else "Reduces"} move time by $seconds seconds for $turns turns. "
    )

case class TimeExtendMult(mult: Double, turns: Int)
    extends TimeExtend
    with SkillEffect(
      s"${mult}x move time for $turns turns. "
    )

case class AttributeChange(turns: Int, att: Attribute)
    extends SkillEffect(s"Change own attribute to $att for $turns turns. ")

trait Haste
case class HasteFixed(turns: Int)
    extends Haste
    with SkillEffect(s"Team skills charged by $turns turns. ")

case class HasteRandom(turnsMin: Int, turnsMax: Int)
    extends Haste
    with SkillEffect(
      s"Team skills charged by $turnsMin-$turnsMax turns at random. "
    )

case class LockOrbs(colors: List[Attribute], numOrbs: Int)
    extends SkillEffect(
      s"Locks ${if (numOrbs < 42) s"$numOrbs " else ""}${colors.mkString(", ")} orbs. "
    )

case class ChangeEnemyAttribute(att: Attribute)
    extends SkillEffect(
      s"Changes all enemies' attribute to $att. "
    )

case class OrbChangeMultiTarget(
    sourceAtts: List[Attribute],
    targetAtts: List[Attribute]
) extends SkillEffect(
      s"Changes ${sourceAtts.mkString(", ")} into a random mix of ${targetAtts.mkString(", ")}"
    )
