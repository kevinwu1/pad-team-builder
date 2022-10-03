package skills.effects.active

import model._
import skills.ActiveSkill

//is this a monoid?
trait SkillEffect(str: String) {
  def and(other: SkillEffect): SkillEffect = {
    (this, other) match {
      case (_, NoEffect) => this
      case (left, c: ConditionalComponent) =>
        throw new Exception(
          "ConditionalComponent can't be on the right of and in: " +
            left + " and " + c
        ) // this is not supposed to happen
      case (MultiEffect(e1), MultiEffect(e2)) =>
        MultiEffect(e1 ::: e2)
      case (MultiEffect(effectsList), e2) =>
        MultiEffect(effectsList :+ e2)
      case (e1, MultiEffect(effectsList2)) =>
        MultiEffect(e1 +: effectsList2)
      case (cond: ConditionalComponent, e2) =>
        ConditionalEffect(cond, e2)
      case (ConditionalEffect(cond, e1), e2) =>
        ConditionalEffect(cond, e1 and e2)
      case _ =>
        MultiEffect(List(this, other))
    }
  }

  override def toString = str
}

object NoEffect extends SkillEffect("") {
  override def and(other: SkillEffect) = other
}

case class MultiEffect(effects: List[SkillEffect])
    extends SkillEffect(
      effects.mkString("\n")
      // effects.zipWithIndex.map((e, i) => s"${i + 1}. $e").mkString("\n")
    )

case class EvolvingEffect(loop: Boolean, skills: List[ActiveSkill])
    extends SkillEffect(
      s"Evolving skill, ${
          if (loop) "loop when end is reached" else "does not loop"
        }: \n${skills.zipWithIndex
          .map((skill, i) => s"Stage ${i + 1} (cd ${skill.cdStr}): \n${skill.skillEffect}")
          .mkString("\n")}"
    )

case class ConditionalEffect(
    condition: ConditionalComponent,
    effect: SkillEffect
) extends SkillEffect(
      s"$condition[\n$effect\n]"
    )

case class ChangeTheWorld(
    seconds: Int
) extends SkillEffect(s"Move orbs freely for ${seconds} seconds. ")

case class CounterAttack(
    multiplier: Int,
    att: Attribute,
    turns: Int
) extends SkillEffect(s"${multiplier}x $att counterattack for $turns turns. ")

trait Suicide

case class SuicidePartial(percentLost: Double)
    extends Suicide
    with SkillEffect(s"HP reduced by ${percentLost}%. ")

object SuicideFull extends Suicide with SkillEffect(s"HP reduced to 1. ")

case class DefenseBreak(
    percent: Int,
    turns: Int
) extends SkillEffect(s"Reduce enemy defense by ${percent}% for $turns turns. ")

case class Delay(
    turns: Int
) extends SkillEffect(s"Delays enemies for ${turns} turns. ")

case class EnhanceOrbs(
    attribute: Attribute
) extends SkillEffect(s"Enhances $attribute orbs. ")

sealed trait Gravity

case class GravityFalse(
    percent: Int
) extends SkillEffect(
      s"Reduce all enemies' current HP by ${percent}% of their current HP. "
    )

case class GravityTrue(percent: Int)
    extends Gravity
    with SkillEffect(
      s"Reduce all enemies' current HP by $percent% of their max HP. "
    )

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

case class HealByTeamRCV(multiplier: Int)
    extends SkillEffect(
      s"Heal ${multiplier}x of entire team's RCV. "
    )

case class HealPerTurn(percent: Int, turns: Int)
    extends Heal
    with SkillEffect(
      s"Heal $percent% max HP every turn for $turns turns. "
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
) extends SkillEffect(s"Changes all orbs to ${attribute.mkString(", ")}. ")
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
    with SkillEffect(s"Randomly spawn ${spawnAtts
        .map(att => s"$numOrbs $att")
        .mkString(", ")} orbs from non [${exclAtts.mkString(", ")}] orbs. ")

case class Poison(
    multiplier: Int
) extends SkillEffect(s"Poisons enemies with ${multiplier}x ATK. ")

object Refresh extends SkillEffect(s"Replaces all orbs. ")

sealed trait RCVBoost
case class RCVBoostMult(multiplier: Double, turns: Int)
    extends RCVBoost
    with SkillEffect(
      s"${multiplier}x RCV for $turns turns. "
    )
case class RCVBoostByAwakening(
    rcvScaling: Double,
    awks: List[Awakening],
    turns: Int
) extends RCVBoost
    with SkillEffect(
      s"1+(${rcvScaling}x) RCV for each ${awks.mkString(", ")} awakening on the team for $turns turns. "
    )

case class RCVBoostByAttributeAndType(
    rcvScaling: Double,
    atts: List[Attribute],
    types: List[CardType],
    turns: Int
) extends RCVBoost
    with SkillEffect(
      s"1+(${rcvScaling}x) RCV for each ${(atts ++ types).mkString(", ")} card on the team for $turns turns. "
    )

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
      s"${multiplier}x ATK for $att for $turns turns. "
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

case class SpikeScalingByAttributeAndType(
    dmgScaling: Double,
    atts: List[Attribute],
    types: List[CardType],
    turns: Int
) extends Spike
    with SkillEffect(
      s"1+(${dmgScaling}x) ATK for each ${(atts ++ types).mkString(", ")} card on the team for $turns turns. "
    )

enum CardSlot(desc: String) {
  case ThisCard extends CardSlot("this card")
  case YourLead extends CardSlot("your leader")
  case FriendLead extends CardSlot("friend leader")
  case AllSubs extends CardSlot("all subs")
  override def toString = desc
}

object CardSlot {
  def from(bits: Int): List[CardSlot] = {
    CardSlot.values.filter(att => (bits & (1 << att.ordinal)) != 0).toList
  }
}

case class SpikeSlots(multiplier: Double, slots: List[CardSlot], turns: Int)
    extends Spike
    with SkillEffect(
      s"${multiplier}x ATK for ${slots.mkString(", ")} for $turns turns. "
    )

trait Transform

case class TransformFixed(
    targetId: Int,
    targetName: String
) extends Transform
    with SkillEffect(s"Transform into #$targetId - $targetName")

case class TransformRandom(targets: List[(Int, String)])
    extends Transform
    with SkillEffect(
      s"Transforms randomly into one of the following: \n${targets.zipWithIndex
          .map((targ, ind) => s"${ind + 1}. ${targ._1} - ${targ._2}")
          .mkString("\n")}"
    )

trait LeadSwap
object LeadSwap
    extends SkillEffect(
      s"Switches places with leader. Switch back when used again. "
    )
object LeadSwapRightMost
    extends SkillEffect(
      "Switches leader and rightmost sub. Switch back when used again."
    )

case class AwokenBindClear(turns: Int)
    extends SkillEffect(s"Awoken bind reduced for $turns turns. ")

case class BindClear(turns: Int)
    extends SkillEffect(s"Bind reduced for $turns turns. ")

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
    extends SkillEffect(s"Changes own attribute to $att for $turns turns. ")

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

trait ChangeEnemyAttribute

case class ChangeEnemyAttributePermanent(att: Attribute)
    extends ChangeEnemyAttribute
    with SkillEffect(
      s"Changes all enemies' attribute to $att. "
    )
case class ChangeEnemyAttributeTemporary(att: Attribute, turns: Int)
    extends ChangeEnemyAttribute
    with SkillEffect(
      s"Changes all enemies' attribute to $att for $turns turns. "
    )

case class OrbChangeMultiTarget(
    sourceAtts: List[Attribute],
    targetAtts: List[Attribute]
) extends SkillEffect(
      s"Changes ${sourceAtts.mkString(", ")} into a random mix of ${targetAtts.mkString(", ")}"
    )

case class AddCombos(combos: Int, turns: Int)
    extends SkillEffect(
      s"Adds $combos combo${if (combos == 1) "" else "s"} for $turns turns. "
    )

trait Void

case class VoidDamageAbsorb(turns: Int)
    extends Void
    with SkillEffect(
      s"Voids damage absorption for $turns turns. "
    )

case class VoidAttributeAbsorb(turns: Int)
    extends Void
    with SkillEffect(
      s"Voids attribute absorption for $turns turns. "
    )
case class VoidVoid(turns: Int)
    extends Void
    with SkillEffect(
      s"Voids damage void for $turns turns. "
    )

case class OrbChangePattern(positions: List[List[Boolean]], att: Attribute)
    extends OrbChange
    with SkillEffect(
      s"Changes the following orbs to $att orbs: \n${Board.boardPositionToCompleteStr(positions)}"
    )

case class EnhancedSkyfall(percent: Int, turns: Int)
    extends SkillEffect(
      s"$percent% chance for enhanced skyfall orbs for $turns turns. "
    )

object OrbTrace
    extends SkillEffect(
      "Unlocks all orbs. \nChanges all orbs to Fire, Water, Wood and Light. \nTraces a 3-combo path on Normal dungeons with 3-linked matches."
    )

case class AllyDelay(turns: Int)
    extends SkillEffect(
      s"Delays team's skills for $turns turns. "
    )

case class AllyDelayRange(minTurns: Int, maxTurns: Int)
    extends SkillEffect(
      s"Delays team's skills for $minTurns-$maxTurns turns. "
    )

object UnlockOrbs extends SkillEffect("Unlock all orbs. ")

case class UnmatchableClear(turns: Int)
    extends SkillEffect(
      s"Unmatchable reduced status by $turns turns. "
    )

case class NoSkyfall(turns: Int)
    extends SkillEffect(
      s"No skyfall combos for $turns turns. "
    )

trait ConditionalComponent

case class ConditionalComponentHP(hpReq: Int, needsToBeMore: Boolean)
    extends ConditionalComponent
    with SkillEffect(
      s"The following activates only if hp is ${
          if (needsToBeMore) "more" else "less"
        } than $hpReq%: "
    )

case class ConditionalComponentFloor(floorReq: Int, needsToBeAfter: Boolean)
    extends ConditionalComponent
    with SkillEffect(
      s"The following activates only if floor is ${
          if (needsToBeAfter) "after" else "before"
        } than $floorReq%: "
    )

trait Spinner

case class SpinnerRandom(numSpinners: Int, speed: Double, turns: Int)
    extends Spinner
    with SkillEffect(
      s"Spawn $numSpinners spinners at random at $speed for $turns turns. "
    )

case class SpinnerFixed(
    positions: List[List[Boolean]],
    speed: Double,
    turns: Int
) extends Spinner
    with SkillEffect(
      s"Spawn spinners in the following positions: \n${Board
          .boardPositionToCompleteStr(positions)} \nat $speed speed for $turns turns."
    )

case class LockedSkyfall(atts: List[Attribute], turns: Int)
    extends SkillEffect(
      s"${atts.mkString(", ")} skyfall locked for $turns turns. "
    )

case class UnableToUseSkills(turns: Int)
    extends SkillEffect(
      s"Unable to use skills for $turns turns. "
    )

case class SelfUnmatchable(atts: List[Attribute], turns: Int)
    extends SkillEffect(
      s"${atts.mkString(", ")} orbs are unmatchable for $turns turns. "
    )

case class NailOrbSkyfall(skyfallChance: Int, turns: Int)
    extends SkillEffect(
      s"$skyfallChance% nail orb skyfall for $turns turns. "
    )

case class MaxHPMult(multiplier: Double, turns: Int)
    extends SkillEffect(
      s"${multiplier}x max HP for $turns turns. "
    )
