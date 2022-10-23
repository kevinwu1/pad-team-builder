package padTeamBuilder.skills.effects.active

import padTeamBuilder.model.Board.BoardPositionExtensions.countPositions
import padTeamBuilder.model.Board.BoardPositions
import padTeamBuilder.model._
import padTeamBuilder.skills.ActiveSkill
// sealed trait SkillEffectJson[T]() {
//   def toJson: JsSuccess = {}
// }

//is this a monoid?
trait SkillEffectGeneric extends Product {
  def <=(that: SkillEffectGeneric): Boolean = SkillEffect.leq(this, that)
  def withNewField(fieldName: String, newValue: Any): SkillEffectGeneric
}

sealed trait SkillEffect(str: String) extends SkillEffectGeneric {
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

object SkillEffect {
  def leq(
      thisEffect: SkillEffectGeneric,
      thatEffect: SkillEffectGeneric
  ): Boolean = {
    val thisClass = thisEffect.getClass
    val thatClass = thatEffect.getClass
    if (thatClass == classOf[MultiEffect]) {
      thatEffect
        .asInstanceOf[MultiEffect]
        .effects
        .exists(e => leq(thisEffect, e))
    } else if (thisClass == classOf[MultiEffect]) {
      thisEffect
        .asInstanceOf[MultiEffect]
        .effects
        .exists(e => leq(e, thatEffect))
    } else
      (
        thisClass.isAssignableFrom(thatClass) ||
          thatClass.isAssignableFrom(thisClass)
      ) && {
        val alwaysMatch = List(
          classOf[NoEffect],
          classOf[MultiEffect],
          classOf[EvolvingEffect],
          classOf[ConditionalEffect],
          classOf[Transform]
        )
        if (
          alwaysMatch.exists(c =>
            c.isAssignableFrom(thisClass) &&
              c.isAssignableFrom(thatClass)
          )
        ) true
        else {
          def getFieldsMap(p: SkillEffectGeneric): Map[String, Any] = {
            p.productElementNames
              .zip(p.productIterator)
              .map((name, value) => name -> value)
              .toMap
          }
          val thisFields = getFieldsMap(thisEffect)
          val thatFields = getFieldsMap(thatEffect)
          def containsAllLeq(
              m1: Map[String, Any],
              m2: Map[String, Any]
          ): Boolean =
            m1.forall((k, v) =>
              m2
                .get(k)
                .map(thatVal => SkillEffect.leq(v, thatVal))
                .getOrElse(false)
            )
          containsAllLeq(thisFields, thatFields)
        }
      }
  }

  def leq(e1: Any, e2: Any): Boolean = {
    if (e1.getClass() != e2.getClass())
      false
    else
      e1 match {
        case _: Int    => e1.asInstanceOf[Int] <= e2.asInstanceOf[Int]
        case _: Double => e1.asInstanceOf[Double] <= e2.asInstanceOf[Double]
        case _: List[_] =>
          e1.asInstanceOf[List[_]].diff(e2.asInstanceOf[List[_]]).isEmpty
        case _ => e1 == e2
      }
  }
}

case class NoEffect() extends SkillEffect("") {
  override def withNewField(
      fieldName: String,
      newValue: Any
  ): SkillEffectGeneric = ???
}

case class MultiEffect(effects: List[SkillEffect])
    extends SkillEffect(
      effects.mkString("\n")
      // effects.zipWithIndex.map((e, i) => s"${i + 1}. $e").mkString("\n")
    ) {
  def withNewField(fieldName: String, newValue: Any): SkillEffectGeneric = ???
}

case class EvolvingEffect(loop: Boolean, skills: List[ActiveSkill])
    extends SkillEffect(
      s"Evolving skill, ${
          if (loop) "loop when end is reached" else "does not loop"
        }: \n${skills.zipWithIndex
          .map((skill, i) => s"Stage ${i + 1} (cd ${skill.cdStr}): \n${skill.skillEffect}")
          .mkString("\n")}"
    ) {
  def withNewField(fieldName: String, newValue: Any): SkillEffectGeneric =
    fieldName match {
      case "loop" => this.copy(loop = newValue.asInstanceOf[Boolean])
    }
}

case class ConditionalEffect(
    condition: ConditionalComponent,
    effect: SkillEffect
) extends SkillEffect(
      s"$condition[\n$effect\n]"
    ) {
  def withNewField(fieldName: String, newValue: Any): SkillEffectGeneric = ???
}

sealed trait ConditionalComponent extends SkillEffectGeneric {
  def withNewField(fieldName: String, newValue: Any): SkillEffectGeneric = ???
}

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

case class ChangeTheWorld(
    seconds: Int
) extends SkillEffect(s"Move orbs freely for ${seconds} seconds. ") {
  def withNewField(fieldName: String, newValue: Any): SkillEffectGeneric =
    fieldName match {
      case "seconds" => this.copy(seconds = newValue.asInstanceOf[Int])
    }
}

case class CounterAttackSkill(
    multiplier: Int,
    att: Attribute,
    turns: Int
) extends SkillEffect(s"${multiplier}x $att counterattack for $turns turns. ") {
  def withNewField(fieldName: String, newValue: Any): SkillEffectGeneric =
    fieldName match {
      case "multiplier" => this.copy(multiplier = newValue.asInstanceOf[Int])
      case "att"        => this.copy(att = newValue.asInstanceOf[Attribute])
      case "turns"      => this.copy(turns = newValue.asInstanceOf[Int])
    }
}

trait Suicide extends SkillEffectGeneric {
  val percentLost: Double
}

case class SuicidePartial(percentLost: Double)
    extends Suicide
    with SkillEffect(s"HP reduced by ${percentLost}%. ")

case class SuicideFull(percentLost: Double = 100.0)
    extends Suicide
    with SkillEffect(s"HP reduced to 1. ")

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

trait Gravity extends SkillEffectGeneric {
  val percent: Int
}

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

sealed trait Heal extends SkillEffectGeneric

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

sealed trait OrbChange extends SkillEffectGeneric {
  val atts: List[Attribute]
}

case class OrbChangeAtoB(
    from: Attribute,
    to: Attribute,
    atts: List[Attribute]
) extends SkillEffect(s"Changes $from orbs to $to orbs. ")
    with OrbChange

object OrbChangeAtoB {
  def apply(from: Attribute, to: Attribute) =
    new OrbChangeAtoB(from, to, List(to))
}

case class OrbChangeFullBoard(
    atts: List[Attribute]
) extends SkillEffect(s"Changes all orbs to ${atts.mkString(", ")}. ")
    with OrbChange

case class OrbChangeColumn(
    column: Column,
    to: Attribute,
    atts: List[Attribute]
) extends OrbChange
    with SkillEffect(s"Changes the $column column into $to. ")

object OrbChangeColumn {
  def apply(column: Column, to: Attribute) =
    new OrbChangeColumn(column, to, List(to))
}

case class OrbChangeColumnRandom(
    column: Column,
    atts: List[Attribute]
) extends OrbChange
    with SkillEffect(
      s"Changes the $column column into a random mix of ${atts.mkString(", ")}. "
    )

case class OrbChangeRow(
    row: Row,
    to: Attribute,
    atts: List[Attribute]
) extends OrbChange
    with SkillEffect(s"Changes the $row row into $to. ")

object OrbChangeRow {
  def apply(row: Row, to: Attribute) =
    new OrbChangeRow(row, to, List(to))
}

case class OrbChangeRandomSpawn(
    numOrbs: Int,
    atts: List[Attribute],
    exclAtts: List[Attribute]
) extends OrbChange
    with SkillEffect(s"Randomly spawn ${atts
        .map(att => s"$numOrbs $att")
        .mkString(", ")} orbs from non [${exclAtts.mkString(", ")}] orbs. ")

case class OrbChangePattern(
    positions: BoardPositions,
    to: Attribute,
    atts: List[Attribute]
) extends OrbChange
    with SkillEffect(
      s"Changes the following orbs to $to orbs: \n${Board.boardPositionsToCompleteStr(positions)}"
    )
object OrbChangePattern {
  def apply(positions: BoardPositions, to: Attribute) =
    new OrbChangePattern(positions, to, List(to))
}

case class OrbChangeMultiTarget(
    sourceAtts: List[Attribute],
    atts: List[Attribute]
) extends OrbChange
    with SkillEffect(
      s"Changes ${sourceAtts.mkString(", ")} into a random mix of ${atts.mkString(", ")}"
    )

case class Poison(
    multiplier: Int
) extends SkillEffect(s"Poisons enemies with ${multiplier}x ATK. ")

case class Refresh() extends SkillEffect(s"Replaces all orbs. ")

sealed trait RCVBoostSkill extends SkillEffectGeneric {
  val turns: Int
}
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

sealed trait Shield extends SkillEffectGeneric {
  val percent: Int
  val turns: Int
}
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
) extends SkillEffect(
      s"${reductionScaling}% damage reduction for each ${awks.mkString(", ")} awakening on the team for $turns turns. "
    ) {}

trait Spike extends SkillEffectGeneric {
  val multiplier: Double
  val turns: Int
}
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

trait SpikeScaling extends SkillEffectGeneric {
  val turns: Int
}

case class SpikeScalingByAwakening(
    dmgScaling: Double,
    awks: List[Awakening],
    turns: Int
) extends SpikeScaling
    with SkillEffect(
      s"1+(${dmgScaling}x) ATK for each ${awks.mkString(", ")} awakening on the team for $turns turns. "
    )

case class SpikeScalingByAttributeAndType(
    dmgScaling: Double,
    atts: List[Attribute],
    types: List[CardType],
    turns: Int
) extends SpikeScaling
    with SkillEffect(
      s"1+(${dmgScaling}x) ATK for each ${(atts ++ types).mkString(", ")} card on the team for $turns turns. "
    )

case class SpikeSlots(multiplier: Double, slots: List[CardSlot], turns: Int)
    extends SkillEffect(
      s"${multiplier}x ATK for ${slots.mkString(", ")} for $turns turns. "
    )

trait Transform extends SkillEffectGeneric

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

trait LeadSwap extends SkillEffectGeneric
case class LeadSwapThisCard()
    extends LeadSwap
    with SkillEffect(
      s"Switches places with leader. Switch back when used again. "
    )
case class LeadSwapRightMost()
    extends LeadSwap
    with SkillEffect(
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

trait TimeExtendSkill extends SkillEffectGeneric {
  val turns: Int
}

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

case class AttributeChangeSelf(turns: Int, att: Attribute)
    extends SkillEffect(s"Changes own attribute to $att for $turns turns. ")

trait Haste extends SkillEffectGeneric {
  val turns: Int
}

case class HasteFixed(turns: Int)
    extends Haste
    with SkillEffect(s"Team skills charged by $turns turns. ")

case class HasteRandom(turns: Int, turnsMax: Int)
    extends Haste
    with SkillEffect(
      s"Team skills charged by $turns-$turnsMax turns at random. "
    )

case class LockOrbs(colors: List[Attribute], numOrbs: Int)
    extends SkillEffect(
      s"Locks ${if (numOrbs < 42) s"$numOrbs " else ""}${colors.mkString(", ")} orbs. "
    )

trait ChangeEnemyAttribute extends SkillEffectGeneric {
  val att: Attribute
}

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

case class AddCombosSkill(combos: Int, turns: Int)
    extends SkillEffect(
      s"Adds $combos combo${if (combos == 1) "" else "s"} for $turns turns. "
    )

trait Void extends SkillEffectGeneric

case class VoidDamageAbsorb(turns: Int)
    extends SkillEffect(
      s"Voids damage absorption for $turns turns. "
    )

case class VoidAttributeAbsorb(turns: Int)
    extends SkillEffect(
      s"Voids attribute absorption for $turns turns. "
    )
case class VoidVoid(turns: Int)
    extends SkillEffect(
      s"Voids damage void for $turns turns. "
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

trait Spinner extends SkillEffectGeneric {
  val numSpinners: Int
  val speed: Double
  val turns: Int
}

case class SpinnerRandom(numSpinners: Int, speed: Double, turns: Int)
    extends Spinner
    with SkillEffect(
      s"Spawn $numSpinners spinners at random at $speed for $turns turns. "
    )

case class SpinnerFixed(
    positions: BoardPositions,
    speed: Double,
    turns: Int,
    numSpinners: Int
) extends Spinner
    with SkillEffect(
      s"Spawn spinners in the following positions: \n${Board
          .boardPositionsToCompleteStr(positions)} \nat $speed speed for $turns turns."
    )

object SpinnerFixed {
  def apply(positions: BoardPositions, speed: Double, turns: Int) = {
    new SpinnerFixed(positions, speed, turns, positions.countPositions)
  }
}

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
