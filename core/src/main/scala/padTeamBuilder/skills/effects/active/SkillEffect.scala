package padTeamBuilder.skills.effects.active

import padTeamBuilder.model._
import padTeamBuilder.skills.ActiveSkill

// sealed trait SkillEffectJson[T]() {
//   def toJson: JsSuccess = {}
// }

//is this a monoid?
sealed trait SkillEffect(str: String) {
  def and(other: SkillEffect): SkillEffect = {
    (this, other) match {
      case (_, NoEffect()) => this
      case (NoEffect(), _) => other
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

case class NoEffect() extends SkillEffect("")

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

case class CounterAttackSkill(
    multiplier: Int,
    att: Attribute,
    turns: Int
) extends SkillEffect(s"${multiplier}x $att counterattack for $turns turns. ")

trait Suicide

case class SuicidePartial(percentLost: Double)
    extends Suicide
    with SkillEffect(s"HP reduced by ${percentLost}%. ")

case class SuicideFull() extends Suicide with SkillEffect(s"HP reduced to 1. ")

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
) extends Gravity
    with SkillEffect(
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
) extends Heal
    with SkillEffect(s"Heal ${amount} HP. ")

case class HealMultiplier(
    multiplier: Int
) extends Heal
    with SkillEffect(s"Heal ${multiplier}x this card's RCV. ")

case class HealPercentMax(
    percent: Int
) extends Heal
    with SkillEffect(s"Heal ${percent}% max HP. ")

case class HealScalingByAwakening(percent: Int, awks: List[Awakening])
    extends Heal
    with SkillEffect(
      s"Heal $percent% RCV as HP for each ${awks.mkString(", ")} awakening on the team. "
    )

case class HealByTeamRCV(multiplier: Int)
    extends Heal
    with SkillEffect(
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

case class OrbChangeColumn(
    column: Column,
    att: Attribute
) extends OrbChange
    with SkillEffect(s"Changes the $column column into $att. ")

case class OrbChangeColumnRandom(
    column: Column,
    atts: List[Attribute]
) extends OrbChange
    with SkillEffect(
      s"Changes the $column column into a random mix of ${atts.mkString(", ")}. "
    )

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

case class Refresh() extends SkillEffect(s"Replaces all orbs. ")

sealed trait RCVBoostSkill
case class RCVBoostMult(multiplier: Double, turns: Int)
    extends RCVBoostSkill
    with SkillEffect(
      s"${multiplier}x RCV for $turns turns. "
    )
case class RCVBoostByAwakening(
    rcvScaling: Double,
    awks: List[Awakening],
    turns: Int
) extends RCVBoostSkill
    with SkillEffect(
      s"1+(${rcvScaling}x) RCV for each ${awks.mkString(", ")} awakening on the team for $turns turns. "
    )

case class RCVBoostByAttributeAndType(
    rcvScaling: Double,
    atts: List[Attribute],
    types: List[CardType],
    turns: Int
) extends RCVBoostSkill
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
case class LeadSwapThisCard()
    extends SkillEffect(
      s"Switches places with leader. Switch back when used again. "
    )
case class LeadSwapRightMost()
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

trait TimeExtendSkill

case class TimeExtendFlat(seconds: Int, turns: Int)
    extends TimeExtendSkill
    with SkillEffect(
      s"${if (seconds > 0) "Extends" else "Reduces"} move time by $seconds seconds for $turns turns. "
    )

case class TimeExtendMult(mult: Double, turns: Int)
    extends TimeExtendSkill
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

case class AddCombosSkill(combos: Int, turns: Int)
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

case class OrbTrace()
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

case class UnlockOrbs() extends SkillEffect("Unlock all orbs. ")

case class UnmatchableClear(turns: Int)
    extends SkillEffect(
      s"Unmatchable reduced status by $turns turns. "
    )

case class NoSkyfallSkill(turns: Int)
    extends SkillEffect(
      s"No skyfall combos for $turns turns. "
    )

sealed trait ConditionalComponent

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

case class ImmediateDamage(
    amount: DAmount,
    damageType: DType,
    target: DTarget,
    drain: Option[Int] = None
) extends SkillEffect({
      val drainText =
        drain.map(p => s" and heal $p% of the damage").getOrElse("")
      s"Inflicts $amount $damageType on $target$drainText. "
    })

sealed trait DAmount(str: String) {
  override def toString = str
}
case class DFixed(amount: Int) extends DAmount(amount.toString)
case class DMultiplier(multiplier: Int) extends DAmount(s"${multiplier}x ATK")
case class DRange(floor: Int, ceil: Int)
    extends DAmount(
      if (floor == ceil) DMultiplier(floor).toString
      else s"${floor}x-${ceil}x ATK"
    )
case class DGrudge(hpMaxMult: Int, hp1Mult: Int)
    extends DAmount(s"[${hpMaxMult}x at full HP, up to ${hp1Mult}x at 1 HP]")
case class DTeamAtkMult(multiplier: Int, atts: List[Attribute])
    extends DAmount(
      s"[${multiplier}x of entire team's ${atts.mkString(" and ")} ATK]"
    )
case class DTeamHpMult(multiplier: Int)
    extends DAmount(
      s"${multiplier}x of entire team's HP"
    )

sealed trait DType(str: String) {
  override def toString = str
}
case class DAttribute(attribute: Attribute)
    extends DType(attribute.toString + " damage")
case class DTrue() extends DType("true damage")
case class DInherit() extends DType("damage")

sealed trait DTarget(str: String) {
  override def toString = str
}
case class DSingle() extends DTarget(s"1 enemy")
case class DAll() extends DTarget(s"all enemies")
case class DAttributeTarget(attribute: Attribute)
    extends DTarget(s"$attribute enemies")

case class TimeReducedForTop2(args: List[Int])
    extends SkillEffect(
      s"Orb move time halved for 1 turn for the top 2 ranked players"
    )
