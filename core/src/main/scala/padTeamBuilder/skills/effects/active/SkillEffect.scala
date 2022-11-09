package padTeamBuilder.skills.effects.active

import padTeamBuilder.model.Board.BoardPositionExtensions.countPositions
import padTeamBuilder.model.Board.BoardPositions
import padTeamBuilder.model._
import padTeamBuilder.skills.ActiveSkill
import padTeamBuilder.util.SkillEffectFieldType
import padTeamBuilder.util.Util

import scala.compiletime._
import scala.deriving.*
//is this a monoid?

sealed trait SkillEffectGeneric extends Product {
  def <=(that: SkillEffectGeneric): Boolean = SkillEffect.leq(this, that)
  def withNewField(fieldName: String, newValue: Any): SkillEffectGeneric
  def getFieldTypes: List[SkillEffectFieldType]
  def canMatch(that: SkillEffectGeneric): Boolean =
    this.getClass == that.getClass
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
    } else if (
      thisEffect.canMatch(thatEffect) || thatEffect.canMatch(thisEffect)
    ) {
      val classes = List(thisEffect, thatEffect)
      classes.exists(cla => {
        cla match {
          case c
              if c == classOf[NoEffect] ||
                c == classOf[MultiEffect] ||
                c == classOf[EvolvingEffect] ||
                c == classOf[ConditionalEffect] ||
                c == classOf[Transform] =>
            true
          case _ => false
        }
      })
      || {
        val commonFields =
          thisEffect.productElementNames.toSet & thatEffect.productElementNames.toSet
        def getFieldsMap(
            p: SkillEffectGeneric,
            commonFields: Set[String]
        ): Map[String, Any] = {
          p.productElementNames
            .zip(p.productIterator)
            .filter(t => commonFields.contains(t._1))
            .map((name, value) => name -> value)
            .toMap
        }
        val thisFields = getFieldsMap(thisEffect, commonFields)
        val thatFields = getFieldsMap(thatEffect, commonFields)
        thisFields.forall((k, v) =>
          thatFields
            .get(k)
            .map(thatVal => SkillEffect.leq(v, thatVal))
            .getOrElse(false)
        )
      }
    } else false
  }

  def leq(e1: Any, e2: Any): Boolean = {
    if (Util.isIntType(e1.getClass))
      e1.asInstanceOf[Int] <= e2.asInstanceOf[Int]
    else {
      if (e1.getClass() != e2.getClass()) {
        println(
          s"CLASSES DONT MATCH???  for $e1 and $e2 of types ${e1
              .getClass()} and ${e2.getClass()}, returning false"
        )
        false
      } else
        e1 match {
          case _: Double => e1.asInstanceOf[Double] <= e2.asInstanceOf[Double]
          case _: List[_] =>
            e1.asInstanceOf[List[_]].diff(e2.asInstanceOf[List[_]]).isEmpty
          case _: Attribute =>
            e1.asInstanceOf[Attribute] == e2.asInstanceOf[Attribute]
            || e1.asInstanceOf[Attribute] == Attribute.NONE
            || e2.asInstanceOf[Attribute] == Attribute.NONE
          case _ => {
            println(
              s"Got unknown types for $e1 and $e2 of types ${e1
                  .getClass()} and ${e2.getClass()}, using basic equality"
            )
            e1 == e2
          }
        }
    }
  }
}

case class NoEffect() extends SkillEffect("") {
  override def getFieldTypes: List[SkillEffectFieldType] =
    Util.getFieldTypes[NoEffect]
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

  override def getFieldTypes: List[SkillEffectFieldType] =
    Util.getFieldTypes[MultiEffect]
  override def withNewField(
      fieldName: String,
      newValue: Any
  ): SkillEffectGeneric =
    ???
}

case class EvolvingEffect(loop: Boolean, skills: List[ActiveSkill])
    extends SkillEffect(
      s"Evolving skill, ${
          if (loop) "loop when end is reached" else "does not loop"
        }: \n${skills.zipWithIndex
          .map((skill, i) => s"Stage ${i + 1} (cd ${skill.cdStr}): \n${skill.skillEffect}")
          .mkString("\n")}"
    ) {
  override def getFieldTypes: List[SkillEffectFieldType] =
    Util.getFieldTypes[EvolvingEffect]
  override def withNewField(
      fieldName: String,
      newValue: Any
  ): SkillEffectGeneric =
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

  override def getFieldTypes: List[SkillEffectFieldType] =
    Util.getFieldTypes[ConditionalEffect]
  override def withNewField(
      fieldName: String,
      newValue: Any
  ): SkillEffectGeneric =
    ???
}

sealed trait ConditionalComponent extends SkillEffectGeneric {

  override def getFieldTypes: List[SkillEffectFieldType] = ???
  override def withNewField(
      fieldName: String,
      newValue: Any
  ): SkillEffectGeneric =
    ???
}

case class ConditionalComponentHP(hpReq: Int, needsToBeMore: Boolean)
    extends ConditionalComponent
    with SkillEffect(
      s"The following activates only if hp is ${
          if (needsToBeMore) "more" else "less"
        } than $hpReq%: "
    ) {}

case class ConditionalComponentFloor(floorReq: Int, needsToBeAfter: Boolean)
    extends ConditionalComponent
    with SkillEffect(
      s"The following activates only if floor is ${
          if (needsToBeAfter) "after" else "before"
        } than $floorReq%: "
    ) {}

case class AddCombosSkill(combos: Int, turns: Int)
    extends SkillEffect(
      s"Adds $combos combo${if (combos == 1) "" else "s"} for $turns turns. "
    ) {
  override def getFieldTypes: List[SkillEffectFieldType] =
    Util.getFieldTypes[AddCombosSkill]
  override def withNewField(
      fieldName: String,
      newValue: Any
  ): SkillEffectGeneric =
    fieldName match {
      case "combos" => this.copy(combos = newValue.asInstanceOf[Int])
      case "turns"  => this.copy(turns = newValue.asInstanceOf[Int])
    }
}

trait AllyDelayEffect {}

case class AllyDelay(turns: Int)
    extends SkillEffect(
      s"Delays team's skills for $turns turns. "
    ) {
  override def getFieldTypes: List[SkillEffectFieldType] =
    Util.getFieldTypes[AllyDelay]
  override def withNewField(
      fieldName: String,
      newValue: Any
  ): SkillEffectGeneric =
    fieldName match {
      case "turns" => this.copy(turns = newValue.asInstanceOf[Int])
    }
}

case class AllyDelayRange(minTurns: Int, maxTurns: Int)
    extends SkillEffect(
      s"Delays team's skills for $minTurns-$maxTurns turns. "
    ) {
  override def getFieldTypes: List[SkillEffectFieldType] =
    Util.getFieldTypes[AllyDelayRange]
  override def withNewField(
      fieldName: String,
      newValue: Any
  ): SkillEffectGeneric =
    fieldName match {
      case "minTurns" => this.copy(minTurns = newValue.asInstanceOf[Int])
      case "maxTurns" => this.copy(maxTurns = newValue.asInstanceOf[Int])
    }
}

sealed trait AttributeChangeEnemyEffect {
  val att: Attribute
}

case class AttributeChangeEnemy(att: Attribute)
    extends AttributeChangeEnemyEffect
    with SkillEffectGeneric {
  override def getFieldTypes: List[SkillEffectFieldType] = List(
    SkillEffectFieldType.OTHER
  )
  override def withNewField(
      fieldName: String,
      newValue: Any
  ): SkillEffectGeneric = {
    fieldName match {
      case "att" => this.copy(att = newValue.asInstanceOf[Attribute])
    }
  }
  override def canMatch(that: SkillEffectGeneric): Boolean = {
    val thatClass = that.getClass
    thatClass == classOf[AttributeChangeEnemyPermanent] || thatClass == classOf[
      AttributeChangeEnemyTemporary
    ]
  }
}

case class AttributeChangeEnemyPermanent(att: Attribute)
    extends AttributeChangeEnemyEffect
    with SkillEffect(
      s"Changes all enemies' attribute to $att. "
    ) {
  override def getFieldTypes: List[SkillEffectFieldType] =
    Util.getFieldTypes[AttributeChangeEnemyPermanent]
  override def withNewField(
      fieldName: String,
      newValue: Any
  ): SkillEffectGeneric =
    fieldName match {
      case "att" => this.copy(att = newValue.asInstanceOf[Attribute])
    }
}

case class AttributeChangeEnemyTemporary(att: Attribute, turns: Int)
    extends AttributeChangeEnemyEffect
    with SkillEffect(
      s"Changes all enemies' attribute to $att for $turns turns. "
    ) {
  override def getFieldTypes: List[SkillEffectFieldType] =
    Util.getFieldTypes[AttributeChangeEnemyTemporary]
  override def withNewField(
      fieldName: String,
      newValue: Any
  ): SkillEffectGeneric =
    fieldName match {
      case "att"   => this.copy(att = newValue.asInstanceOf[Attribute])
      case "turns" => this.copy(turns = newValue.asInstanceOf[Int])
    }
}

case class AttributeChangeSelf(turns: Int, att: Attribute)
    extends SkillEffect(s"Changes own attribute to $att for $turns turns. ") {
  override def getFieldTypes: List[SkillEffectFieldType] =
    Util.getFieldTypes[AttributeChangeSelf]
  override def withNewField(
      fieldName: String,
      newValue: Any
  ): SkillEffectGeneric =
    fieldName match {
      case "turns" => this.copy(turns = newValue.asInstanceOf[Int])
      case "att"   => this.copy(att = newValue.asInstanceOf[Attribute])
    }
}

case class AwokenBindClear(turns: Int)
    extends SkillEffect(s"Awoken bind reduced for $turns turns. ") {
  override def getFieldTypes: List[SkillEffectFieldType] =
    Util.getFieldTypes[AwokenBindClear]
  override def withNewField(
      fieldName: String,
      newValue: Any
  ): SkillEffectGeneric =
    fieldName match {
      case "turns" => this.copy(turns = newValue.asInstanceOf[Int])
    }
}

case class BindClear(turns: Int)
    extends SkillEffect(s"Bind reduced for $turns turns. ") {
  override def getFieldTypes: List[SkillEffectFieldType] =
    Util.getFieldTypes[BindClear]
  override def withNewField(
      fieldName: String,
      newValue: Any
  ): SkillEffectGeneric =
    fieldName match {
      case "turns" => this.copy(turns = newValue.asInstanceOf[Int])
    }
}

case class ChangeTheWorld(
    seconds: Int
) extends SkillEffect(s"Move orbs freely for ${seconds} seconds. ") {
  override def getFieldTypes: List[SkillEffectFieldType] =
    Util.getFieldTypes[ChangeTheWorld]
  override def withNewField(
      fieldName: String,
      newValue: Any
  ): SkillEffectGeneric =
    fieldName match {
      case "seconds" => this.copy(seconds = newValue.asInstanceOf[Int])
    }
}

case class CounterAttackSkill(
    multiplier: Int,
    att: Attribute,
    turns: Int
) extends SkillEffect(s"${multiplier}x $att counterattack for $turns turns. ") {
  override def getFieldTypes: List[SkillEffectFieldType] =
    Util.getFieldTypes[CounterAttackSkill]
  override def withNewField(
      fieldName: String,
      newValue: Any
  ): SkillEffectGeneric =
    fieldName match {
      case "multiplier" => this.copy(multiplier = newValue.asInstanceOf[Int])
      case "att"        => this.copy(att = newValue.asInstanceOf[Attribute])
      case "turns"      => this.copy(turns = newValue.asInstanceOf[Int])
    }
}

case class DefenseBreak(
    percent: Int,
    turns: Int
) extends SkillEffect(
      s"Reduce enemy defense by ${percent}% for $turns turns. "
    ) {
  override def getFieldTypes: List[SkillEffectFieldType] =
    Util.getFieldTypes[DefenseBreak]
  override def withNewField(
      fieldName: String,
      newValue: Any
  ): SkillEffectGeneric =
    fieldName match {
      case "percent" => this.copy(percent = newValue.asInstanceOf[Int])
      case "turns"   => this.copy(turns = newValue.asInstanceOf[Int])
    }
}

case class Delay(
    turns: Int
) extends SkillEffect(s"Delays enemies for ${turns} turns. ") {
  override def getFieldTypes: List[SkillEffectFieldType] =
    Util.getFieldTypes[Delay]
  override def withNewField(
      fieldName: String,
      newValue: Any
  ): SkillEffectGeneric =
    fieldName match {
      case "turns" => this.copy(turns = newValue.asInstanceOf[Int])
    }
}

case class EnhanceOrbs(
    attribute: Attribute
) extends SkillEffect(s"Enhances $attribute orbs. ") {
  override def getFieldTypes: List[SkillEffectFieldType] =
    Util.getFieldTypes[EnhanceOrbs]
  override def withNewField(
      fieldName: String,
      newValue: Any
  ): SkillEffectGeneric =
    fieldName match {
      case "attribute" =>
        this.copy(attribute = newValue.asInstanceOf[Attribute])
    }
}

case class EnhancedSkyfall(percent: Int, turns: Int)
    extends SkillEffect(
      s"$percent% chance for enhanced skyfall orbs for $turns turns. "
    ) {
  override def getFieldTypes: List[SkillEffectFieldType] =
    Util.getFieldTypes[EnhancedSkyfall]
  override def withNewField(
      fieldName: String,
      newValue: Any
  ): SkillEffectGeneric =
    fieldName match {
      case "percent" => this.copy(percent = newValue.asInstanceOf[Int])
      case "turns"   => this.copy(turns = newValue.asInstanceOf[Int])
    }
}

sealed trait Gravity {
  val percent: Int
}

case class GravityGeneric(percent: Int)
    extends Gravity
    with SkillEffectGeneric {

  override def getFieldTypes: List[SkillEffectFieldType] = List(
    SkillEffectFieldType.INT
  )
  override def withNewField(
      fieldName: String,
      newValue: Any
  ): SkillEffectGeneric = {
    fieldName match {
      case "percent" => this.copy(percent = newValue.asInstanceOf[Int])
    }
  }

  override def canMatch(that: SkillEffectGeneric): Boolean = {
    val thatClass = that.getClass
    thatClass == classOf[GravityFalse] || thatClass == classOf[GravityTrue]
  }
}

case class GravityFalse(
    percent: Int
) extends Gravity
    with SkillEffect(
      s"Reduce all enemies' current HP by ${percent}% of their current HP. "
    ) {
  override def getFieldTypes: List[SkillEffectFieldType] =
    Util.getFieldTypes[GravityFalse]
  override def withNewField(
      fieldName: String,
      newValue: Any
  ): SkillEffectGeneric =
    fieldName match {
      case "percent" => this.copy(percent = newValue.asInstanceOf[Int])
    }
}

case class GravityTrue(percent: Int)
    extends Gravity
    with SkillEffect(
      s"Reduce all enemies' current HP by $percent% of their max HP. "
    ) {
  override def getFieldTypes: List[SkillEffectFieldType] =
    Util.getFieldTypes[GravityTrue]
  override def withNewField(
      fieldName: String,
      newValue: Any
  ): SkillEffectGeneric =
    fieldName match {
      case "percent" => this.copy(percent = newValue.asInstanceOf[Int])
    }
}
sealed trait HasteEffect {
  val turns: Int
}

case class Haste(override val turns: Int)
    extends HasteEffect
    with SkillEffectGeneric {
  override def getFieldTypes: List[SkillEffectFieldType] = List(
    SkillEffectFieldType.INT
  )
  override def withNewField(
      fieldName: String,
      newValue: Any
  ): SkillEffectGeneric = {
    fieldName match {
      case "turns" => this.copy(turns = newValue.asInstanceOf[Int])
    }
  }
  override def canMatch(that: SkillEffectGeneric): Boolean = {
    val thatClass = that.getClass
    thatClass == classOf[HasteFixed] || thatClass == classOf[HasteRandom]
  }
}

case class HasteFixed(override val turns: Int)
    extends HasteEffect
    with SkillEffect(s"Team skills charged by $turns turns. ") {
  override def getFieldTypes: List[SkillEffectFieldType] =
    Util.getFieldTypes[HasteFixed]
  override def withNewField(
      fieldName: String,
      newValue: Any
  ): SkillEffectGeneric =
    fieldName match {
      case "turns" => this.copy(turns = newValue.asInstanceOf[Int])
    }
}

case class HasteRandom(override val turns: Int, turnsMax: Int)
    extends HasteEffect
    with SkillEffect(
      s"Team skills charged by $turns-$turnsMax turns at random. "
    ) {
  override def getFieldTypes: List[SkillEffectFieldType] =
    Util.getFieldTypes[HasteRandom]
  override def withNewField(
      fieldName: String,
      newValue: Any
  ): SkillEffectGeneric =
    fieldName match {
      case "turns"    => this.copy(turns = newValue.asInstanceOf[Int])
      case "turnsMax" => this.copy(turnsMax = newValue.asInstanceOf[Int])
    }
}

sealed trait Heal {}

case class HealGeneric() extends Heal with SkillEffectGeneric {
  override def getFieldTypes: List[SkillEffectFieldType] = List()

  override def withNewField(
      fieldName: String,
      newValue: Any
  ): SkillEffectGeneric = ???

  override def canMatch(that: SkillEffectGeneric): Boolean = {
    val thatClass = that.getClass
    thatClass == classOf[HealFlat]
    || thatClass == classOf[HealMultiplier]
    || thatClass == classOf[HealPercentMax]
    || thatClass == classOf[HealScalingByAwakening]
    || thatClass == classOf[HealByTeamRCV]
    || thatClass == classOf[HealPerTurn]
  }
}

case class HealFlat(
    amount: Int
) extends Heal
    with SkillEffect(s"Heal ${amount} HP. ") {
  override def getFieldTypes: List[SkillEffectFieldType] =
    Util.getFieldTypes[HealFlat]
  override def withNewField(
      fieldName: String,
      newValue: Any
  ): SkillEffectGeneric =
    fieldName match {
      case "amount" => this.copy(amount = newValue.asInstanceOf[Int])
    }
}

case class HealMultiplier(
    multiplier: Int
) extends Heal
    with SkillEffect(s"Heal ${multiplier}x this card's RCV. ") {
  override def getFieldTypes: List[SkillEffectFieldType] =
    Util.getFieldTypes[HealMultiplier]
  override def withNewField(
      fieldName: String,
      newValue: Any
  ): SkillEffectGeneric =
    fieldName match {
      case "multiplier" => this.copy(multiplier = newValue.asInstanceOf[Int])
    }
}

case class HealPercentMax(
    percent: Int
) extends Heal
    with SkillEffect(s"Heal ${percent}% max HP. ") {
  override def getFieldTypes: List[SkillEffectFieldType] =
    Util.getFieldTypes[HealPercentMax]
  override def withNewField(
      fieldName: String,
      newValue: Any
  ): SkillEffectGeneric =
    fieldName match {
      case "percent" => this.copy(percent = newValue.asInstanceOf[Int])
    }
}

case class HealScalingByAwakening(percent: Int, awks: List[Awakening])
    extends Heal
    with SkillEffect(
      s"Heal $percent% RCV as HP for each ${awks.mkString(", ")} awakening on the team. "
    ) {
  override def getFieldTypes: List[SkillEffectFieldType] =
    Util.getFieldTypes[HealScalingByAwakening]
  override def withNewField(
      fieldName: String,
      newValue: Any
  ): SkillEffectGeneric =
    fieldName match {
      case "percent" => this.copy(percent = newValue.asInstanceOf[Int])
      case "awks"    => this.copy(awks = newValue.asInstanceOf[List[Awakening]])
    }
}

case class HealByTeamRCV(multiplier: Int)
    extends Heal
    with SkillEffect(
      s"Heal ${multiplier}x of entire team's RCV. "
    ) {
  override def getFieldTypes: List[SkillEffectFieldType] =
    Util.getFieldTypes[HealByTeamRCV]
  override def withNewField(
      fieldName: String,
      newValue: Any
  ): SkillEffectGeneric =
    fieldName match {
      case "multiplier" => this.copy(multiplier = newValue.asInstanceOf[Int])
    }
}

case class HealPerTurn(percent: Int, turns: Int)
    extends Heal
    with SkillEffect(
      s"Heal $percent% max HP every turn for $turns turns. "
    ) {
  override def getFieldTypes: List[SkillEffectFieldType] =
    Util.getFieldTypes[HealPerTurn]
  override def withNewField(
      fieldName: String,
      newValue: Any
  ): SkillEffectGeneric =
    fieldName match {
      case "percent" => this.copy(percent = newValue.asInstanceOf[Int])
      case "turns"   => this.copy(turns = newValue.asInstanceOf[Int])
    }
}

case class ImmediateDamage(
    amount: DAmount,
    damageType: DType,
    target: DTarget,
    drain: Option[Int] = None
) extends SkillEffect({
      val drainText =
        drain.map(p => s" and heal $p% of the damage").getOrElse("")
      s"Inflicts $amount $damageType on $target$drainText. "
    }) {
  override def getFieldTypes: List[SkillEffectFieldType] =
    Util.getFieldTypes[ImmediateDamage]
  override def withNewField(
      fieldName: String,
      newValue: Any
  ): SkillEffectGeneric =
    fieldName match {
      case "amount"     => this.copy(amount = newValue.asInstanceOf[DAmount])
      case "damageType" => this.copy(damageType = newValue.asInstanceOf[DType])
      case "target"     => this.copy(target = newValue.asInstanceOf[DTarget])
      case "drain"      => this.copy(drain = newValue.asInstanceOf[Option[Int]])
    }
}

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

case class IncreaseSkyfall(
    colors: List[Attribute],
    percent: Int,
    turns: Int
) extends SkillEffect(
      s"$percent% increased skyfall for ${colors.mkString(", ")} for $turns turns. "
    ) {
  override def getFieldTypes: List[SkillEffectFieldType] =
    Util.getFieldTypes[IncreaseSkyfall]
  override def withNewField(
      fieldName: String,
      newValue: Any
  ): SkillEffectGeneric =
    fieldName match {
      case "colors" =>
        this.copy(colors = newValue.asInstanceOf[List[Attribute]])
      case "percent" => this.copy(percent = newValue.asInstanceOf[Int])
      case "turns"   => this.copy(turns = newValue.asInstanceOf[Int])
    }
}

sealed trait LeadSwap {}

case class LeadSwapGeneric() extends LeadSwap with SkillEffectGeneric {
  override def getFieldTypes: List[SkillEffectFieldType] = List()
  override def withNewField(
      fieldName: String,
      newValue: Any
  ): SkillEffectGeneric = ???
  override def canMatch(that: SkillEffectGeneric): Boolean = {
    val thatClass = that.getClass
    thatClass == classOf[LeadSwapThisCard]
    || thatClass == classOf[LeadSwapRightMost]
  }
}
case class LeadSwapThisCard()
    extends LeadSwap
    with SkillEffect(
      s"Switches places with leader. Switch back when used again. "
    ) {
  override def getFieldTypes: List[SkillEffectFieldType] =
    Util.getFieldTypes[LeadSwapThisCard]
  override def withNewField(
      fieldName: String,
      newValue: Any
  ): SkillEffectGeneric =
    ???
}

case class LeadSwapRightMost()
    extends LeadSwap
    with SkillEffect(
      "Switches leader and rightmost sub. Switch back when used again."
    ) {
  override def getFieldTypes: List[SkillEffectFieldType] =
    Util.getFieldTypes[LeadSwapRightMost]
  override def withNewField(
      fieldName: String,
      newValue: Any
  ): SkillEffectGeneric =
    ???
}

case class LockedSkyfall(atts: List[Attribute], turns: Int)
    extends SkillEffect(
      s"${atts.mkString(", ")} skyfall locked for $turns turns. "
    ) {

  override def getFieldTypes: List[SkillEffectFieldType] =
    Util.getFieldTypes[LockedSkyfall]
  override def withNewField(
      fieldName: String,
      newValue: Any
  ): SkillEffectGeneric =
    fieldName match {
      case "atts"  => this.copy(atts = newValue.asInstanceOf[List[Attribute]])
      case "turns" => this.copy(turns = newValue.asInstanceOf[Int])
    }
}

case class LockOrbs(colors: List[Attribute], numOrbs: Int)
    extends SkillEffect(
      s"Locks ${if (numOrbs < 42) s"$numOrbs " else ""}${colors.mkString(", ")} orbs. "
    ) {
  override def getFieldTypes: List[SkillEffectFieldType] =
    Util.getFieldTypes[LockOrbs]
  override def withNewField(
      fieldName: String,
      newValue: Any
  ): SkillEffectGeneric =
    fieldName match {
      case "colors" =>
        this.copy(colors = newValue.asInstanceOf[List[Attribute]])
      case "numOrbs" => this.copy(numOrbs = newValue.asInstanceOf[Int])
    }
}

case class MassAttack(
    turns: Int
) extends SkillEffect(s"Mass attacks for $turns turns. ") {
  override def getFieldTypes: List[SkillEffectFieldType] =
    Util.getFieldTypes[MassAttack]
  override def withNewField(
      fieldName: String,
      newValue: Any
  ): SkillEffectGeneric =
    fieldName match {
      case "turns" => this.copy(turns = newValue.asInstanceOf[Int])
    }
}

case class MaxHPMult(multiplier: Double, turns: Int)
    extends SkillEffect(
      s"${multiplier}x max HP for $turns turns. "
    ) {
  override def getFieldTypes: List[SkillEffectFieldType] =
    Util.getFieldTypes[MaxHPMult]
  override def withNewField(
      fieldName: String,
      newValue: Any
  ): SkillEffectGeneric =
    fieldName match {
      case "multiplier" => this.copy(multiplier = newValue.asInstanceOf[Double])
      case "turns"      => this.copy(turns = newValue.asInstanceOf[Int])
    }
}
case class NailOrbSkyfall(skyfallChance: Int, turns: Int)
    extends SkillEffect(
      s"$skyfallChance% nail orb skyfall for $turns turns. "
    ) {

  override def getFieldTypes: List[SkillEffectFieldType] =
    Util.getFieldTypes[NailOrbSkyfall]
  override def withNewField(
      fieldName: String,
      newValue: Any
  ): SkillEffectGeneric =
    fieldName match {
      case "skyfallChance" =>
        this.copy(skyfallChance = newValue.asInstanceOf[Int])
      case "turns" => this.copy(turns = newValue.asInstanceOf[Int])
    }
}

case class NoSkyfallSkill(turns: Int)
    extends SkillEffect(
      s"No skyfall combos for $turns turns. "
    ) {

  override def getFieldTypes: List[SkillEffectFieldType] =
    Util.getFieldTypes[NoSkyfallSkill]
  override def withNewField(
      fieldName: String,
      newValue: Any
  ): SkillEffectGeneric =
    fieldName match {
      case "turns" => this.copy(turns = newValue.asInstanceOf[Int])
    }
}

sealed trait OrbChange {
  val atts: List[Attribute]
}

case class OrbChangeGeneric(atts: List[Attribute])
    extends OrbChange
    with SkillEffectGeneric {
  override def getFieldTypes: List[SkillEffectFieldType] = List(
    SkillEffectFieldType.OTHER
  )
  override def withNewField(
      fieldName: String,
      newValue: Any
  ): SkillEffectGeneric = {
    fieldName match {
      case "atts" => this.copy(atts = newValue.asInstanceOf[List[Attribute]])
    }
  }
  override def canMatch(that: SkillEffectGeneric): Boolean = {
    val thatClass = that.getClass
    thatClass == classOf[OrbChangeAtoB]
    || thatClass == classOf[OrbChangeFullBoard]
    || thatClass == classOf[OrbChangeColumn]
    || thatClass == classOf[OrbChangeColumnRandom]
    || thatClass == classOf[OrbChangeRow]
    || thatClass == classOf[OrbChangeRandomSpawn]
    || thatClass == classOf[OrbChangePattern]
    || thatClass == classOf[OrbChangeMultiTarget]
  }
}

case class OrbChangeAtoB(
    from: Attribute,
    to: Attribute,
    atts: List[Attribute]
) extends OrbChange
    with SkillEffect(s"Changes $from orbs to $to orbs. ") {
  override def getFieldTypes: List[SkillEffectFieldType] =
    Util.getFieldTypes[OrbChangeAtoB]
  override def withNewField(
      fieldName: String,
      newValue: Any
  ): SkillEffectGeneric =
    fieldName match {
      case "from" => this.copy(from = newValue.asInstanceOf[Attribute])
      case "to"   => this.copy(to = newValue.asInstanceOf[Attribute])
      case "atts" => this.copy(atts = newValue.asInstanceOf[List[Attribute]])
    }
}

object OrbChangeAtoB {
  def apply(from: Attribute, to: Attribute) =
    new OrbChangeAtoB(from, to, List(to))
}

case class OrbChangeFullBoard(
    atts: List[Attribute]
) extends OrbChange
    with SkillEffect(s"Changes all orbs to ${atts.mkString(", ")}. ") {
  override def getFieldTypes: List[SkillEffectFieldType] =
    Util.getFieldTypes[OrbChangeFullBoard]
  override def withNewField(
      fieldName: String,
      newValue: Any
  ): SkillEffectGeneric =
    fieldName match {
      case "atts" => this.copy(atts = newValue.asInstanceOf[List[Attribute]])
    }
}

case class OrbChangeColumn(
    column: Column,
    to: Attribute,
    atts: List[Attribute]
) extends OrbChange
    with SkillEffect(s"Changes the $column column into $to. ") {
  override def getFieldTypes: List[SkillEffectFieldType] =
    Util.getFieldTypes[OrbChangeColumn]
  override def withNewField(
      fieldName: String,
      newValue: Any
  ): SkillEffectGeneric =
    fieldName match {
      case "column" => this.copy(column = newValue.asInstanceOf[Column])
      case "to"     => this.copy(to = newValue.asInstanceOf[Attribute])
      case "atts"   => this.copy(atts = newValue.asInstanceOf[List[Attribute]])
    }
}

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
    ) {
  override def getFieldTypes: List[SkillEffectFieldType] =
    Util.getFieldTypes[OrbChangeColumnRandom]
  override def withNewField(
      fieldName: String,
      newValue: Any
  ): SkillEffectGeneric =
    fieldName match {
      case "column" => this.copy(column = newValue.asInstanceOf[Column])
      case "atts"   => this.copy(atts = newValue.asInstanceOf[List[Attribute]])
    }
}

case class OrbChangeRow(
    row: Row,
    to: Attribute,
    atts: List[Attribute]
) extends OrbChange
    with SkillEffect(s"Changes the $row row into $to. ") {
  override def getFieldTypes: List[SkillEffectFieldType] =
    Util.getFieldTypes[OrbChangeRow]
  override def withNewField(
      fieldName: String,
      newValue: Any
  ): SkillEffectGeneric =
    fieldName match {
      case "row"  => this.copy(row = newValue.asInstanceOf[Row])
      case "to"   => this.copy(to = newValue.asInstanceOf[Attribute])
      case "atts" => this.copy(atts = newValue.asInstanceOf[List[Attribute]])
    }
}

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
        .mkString(", ")} orbs${
        if (exclAtts.nonEmpty) s" from non [${exclAtts.mkString(", ")}] orbs"
        else ""
      }. ") {
  override def getFieldTypes: List[SkillEffectFieldType] =
    Util.getFieldTypes[OrbChangeRow]
  override def withNewField(
      fieldName: String,
      newValue: Any
  ): SkillEffectGeneric =
    fieldName match {
      case "numOrbs" => this.copy(numOrbs = newValue.asInstanceOf[Int])
      case "atts"    => this.copy(atts = newValue.asInstanceOf[List[Attribute]])
      case "exclAtts" =>
        this.copy(exclAtts = newValue.asInstanceOf[List[Attribute]])
    }
}

case class OrbChangePattern(
    positions: BoardPositions,
    to: Attribute,
    atts: List[Attribute]
) extends OrbChange
    with SkillEffect(
      s"Changes the following orbs to $to orbs: \n${Board.boardPositionsToCompleteStr(positions)}"
    ) {
  override def getFieldTypes: List[SkillEffectFieldType] =
    Util.getFieldTypes[OrbChangePattern]
  override def withNewField(
      fieldName: String,
      newValue: Any
  ): SkillEffectGeneric =
    fieldName match {
      case "positions" =>
        this.copy(positions = newValue.asInstanceOf[BoardPositions])
      case "to"   => this.copy(to = newValue.asInstanceOf[Attribute])
      case "atts" => this.copy(atts = newValue.asInstanceOf[List[Attribute]])
    }
}

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
    ) {
  override def getFieldTypes: List[SkillEffectFieldType] =
    Util.getFieldTypes[OrbChangeMultiTarget]
  override def withNewField(
      fieldName: String,
      newValue: Any
  ): SkillEffectGeneric =
    fieldName match {
      case "sourceAtts" =>
        this.copy(sourceAtts = newValue.asInstanceOf[List[Attribute]])
      case "atts" => this.copy(atts = newValue.asInstanceOf[List[Attribute]])
    }
}

case class OrbTrace()
    extends SkillEffect(
      "Unlocks all orbs. \nChanges all orbs to Fire, Water, Wood and Light. \nTraces a 3-combo path on Normal dungeons with 3-linked matches."
    ) {

  override def getFieldTypes: List[SkillEffectFieldType] =
    Util.getFieldTypes[OrbTrace]
  override def withNewField(
      fieldName: String,
      newValue: Any
  ): SkillEffectGeneric =
    ???
}

case class Poison(
    multiplier: Int
) extends SkillEffect(s"Poisons enemies with ${multiplier}x ATK. ") {
  override def getFieldTypes: List[SkillEffectFieldType] =
    Util.getFieldTypes[Poison]
  override def withNewField(
      fieldName: String,
      newValue: Any
  ): SkillEffectGeneric =
    fieldName match {
      case "multiplier" => this.copy(multiplier = newValue.asInstanceOf[Int])
    }
}

case class Random(effects: List[ActiveSkill])
    extends SkillEffect({
      val skills =
        effects.zipWithIndex.map((s, i) => s"${i + 1}. $s").mkString("\n")
      s"Randomly activates one of the following: \n$skills"
    }) {
  override def getFieldTypes: List[SkillEffectFieldType] =
    Util.getFieldTypes[Random]
  override def withNewField(
      fieldName: String,
      newValue: Any
  ): SkillEffectGeneric =
    ???
}

case class Refresh() extends SkillEffect(s"Replaces all orbs. ") {
  override def getFieldTypes: List[SkillEffectFieldType] =
    Util.getFieldTypes[Refresh]
  override def withNewField(
      fieldName: String,
      newValue: Any
  ): SkillEffectGeneric =
    ???
}

sealed trait RCVBoostSkill {
  val turns: Int
}

case class RCVBoostSkillGeneric(turns: Int)
    extends RCVBoostSkill
    with SkillEffectGeneric {
  override def getFieldTypes: List[SkillEffectFieldType] = List(
    SkillEffectFieldType.INT
  )
  override def withNewField(
      fieldName: String,
      newValue: Any
  ): SkillEffectGeneric = {
    fieldName match {
      case "turns" => this.copy(turns = newValue.asInstanceOf[Int])
    }
  }
  override def canMatch(that: SkillEffectGeneric): Boolean = {
    val thatClass = that.getClass
    thatClass == classOf[RCVBoostMult]
    || thatClass == classOf[RCVBoostByAwakening]
    || thatClass == classOf[RCVBoostByAttributeAndType]
  }
}
case class RCVBoostMult(multiplier: Double, turns: Int)
    extends RCVBoostSkill
    with SkillEffect(
      s"${multiplier}x RCV for $turns turns. "
    ) {
  override def getFieldTypes: List[SkillEffectFieldType] =
    Util.getFieldTypes[RCVBoostMult]
  override def withNewField(
      fieldName: String,
      newValue: Any
  ): SkillEffectGeneric =
    fieldName match {
      case "multiplier" => this.copy(multiplier = newValue.asInstanceOf[Double])
      case "turns"      => this.copy(turns = newValue.asInstanceOf[Int])
    }
}

case class RCVBoostByAwakening(
    rcvScaling: Double,
    awks: List[Awakening],
    turns: Int
) extends RCVBoostSkill
    with SkillEffect(
      s"1+(${rcvScaling}x) RCV for each ${awks.mkString(", ")} awakening on the team for $turns turns. "
    ) {
  override def getFieldTypes: List[SkillEffectFieldType] =
    Util.getFieldTypes[RCVBoostByAwakening]
  override def withNewField(
      fieldName: String,
      newValue: Any
  ): SkillEffectGeneric =
    fieldName match {
      case "rcvScaling" => this.copy(rcvScaling = newValue.asInstanceOf[Double])
      case "awks"  => this.copy(awks = newValue.asInstanceOf[List[Awakening]])
      case "turns" => this.copy(turns = newValue.asInstanceOf[Int])
    }
}

case class RCVBoostByAttributeAndType(
    rcvScaling: Double,
    atts: List[Attribute],
    types: List[CardType],
    turns: Int
) extends RCVBoostSkill
    with SkillEffect(
      s"1+(${rcvScaling}x) RCV for each ${(atts ++ types).mkString(", ")} card on the team for $turns turns. "
    ) {
  override def getFieldTypes: List[SkillEffectFieldType] =
    Util.getFieldTypes[RCVBoostByAttributeAndType]
  override def withNewField(
      fieldName: String,
      newValue: Any
  ): SkillEffectGeneric =
    fieldName match {
      case "rcvScaling" => this.copy(rcvScaling = newValue.asInstanceOf[Double])
      case "atts"  => this.copy(atts = newValue.asInstanceOf[List[Attribute]])
      case "types" => this.copy(types = newValue.asInstanceOf[List[CardType]])
      case "turns" => this.copy(turns = newValue.asInstanceOf[Int])
    }
}
case class SelfUnmatchable(atts: List[Attribute], turns: Int)
    extends SkillEffect(
      s"${atts.mkString(", ")} orbs are unmatchable for $turns turns. "
    ) {

  override def getFieldTypes: List[SkillEffectFieldType] =
    Util.getFieldTypes[SelfUnmatchable]
  override def withNewField(
      fieldName: String,
      newValue: Any
  ): SkillEffectGeneric =
    fieldName match {
      case "atts"  => this.copy(atts = newValue.asInstanceOf[List[Attribute]])
      case "turns" => this.copy(turns = newValue.asInstanceOf[Int])
    }
}

sealed trait Shield {
  val percent: Int
  val turns: Int
}

case class ShieldAll(
    percent: Int,
    turns: Int
) extends Shield
    with SkillEffect(
      s"Reduces damage taken by ${percent}% for $turns turns. "
    ) {
  override def getFieldTypes: List[SkillEffectFieldType] =
    Util.getFieldTypes[ShieldAll]
  override def withNewField(
      fieldName: String,
      newValue: Any
  ): SkillEffectGeneric =
    fieldName match {
      case "percent" => this.copy(percent = newValue.asInstanceOf[Int])
      case "turns"   => this.copy(turns = newValue.asInstanceOf[Int])
    }
}

case class ShieldAttribute(
    turns: Int,
    attribute: Attribute,
    percent: Int
) extends Shield
    with SkillEffect(
      s"For $turns turns, ${percent}% reduced $attribute damage taken"
    ) {
  override def getFieldTypes: List[SkillEffectFieldType] =
    Util.getFieldTypes[ShieldAttribute]
  override def withNewField(
      fieldName: String,
      newValue: Any
  ): SkillEffectGeneric =
    fieldName match {
      case "turns" => this.copy(turns = newValue.asInstanceOf[Int])
      case "attribute" =>
        this.copy(attribute = newValue.asInstanceOf[Attribute])
      case "percent" => this.copy(percent = newValue.asInstanceOf[Int])
    }
}

case class ShieldScalingByAwakening(
    reductionScaling: Double,
    awks: List[Awakening],
    turns: Int
) extends SkillEffect(
      s"${reductionScaling}% damage reduction for each ${awks.mkString(", ")} awakening on the team for $turns turns. "
    ) {
  override def getFieldTypes: List[SkillEffectFieldType] =
    Util.getFieldTypes[ShieldScalingByAwakening]
  override def withNewField(
      fieldName: String,
      newValue: Any
  ): SkillEffectGeneric =
    fieldName match {
      case "reductionScaling" =>
        this.copy(reductionScaling = newValue.asInstanceOf[Double])
      case "awks"  => this.copy(awks = newValue.asInstanceOf[List[Awakening]])
      case "turns" => this.copy(turns = newValue.asInstanceOf[Int])
    }
}

sealed trait Spike extends SkillEffectGeneric {
  val multiplier: Double
  val turns: Int
}

case class SpikeGeneric(multiplier: Double, turns: Int)
    extends Spike
    with SkillEffectGeneric {
  override def getFieldTypes: List[SkillEffectFieldType] = List(
    SkillEffectFieldType.DOUBLE,
    SkillEffectFieldType.INT
  )
  override def withNewField(
      fieldName: String,
      newValue: Any
  ): SkillEffectGeneric = {
    fieldName match {
      case "multiplier" => this.copy(multiplier = newValue.asInstanceOf[Double])
      case "turns"      => this.copy(turns = newValue.asInstanceOf[Int])
    }
  }
  override def canMatch(that: SkillEffectGeneric): Boolean = {
    val thatClass = that.getClass
    thatClass == classOf[SpikeAttribute]
    || thatClass == classOf[SpikeType]
    || thatClass == classOf[SpikeScalingByAwakening]
    || thatClass == classOf[SpikeScalingByAttributeAndType]
    || thatClass == classOf[SpikeSlots]
  }
}

case class SpikeAttribute(
    multiplier: Double,
    att: Attribute,
    turns: Int
) extends Spike
    with SkillEffect(
      s"${multiplier}x ATK for $att for $turns turns. "
    ) {
  override def getFieldTypes: List[SkillEffectFieldType] =
    Util.getFieldTypes[SpikeAttribute]
  override def withNewField(
      fieldName: String,
      newValue: Any
  ): SkillEffectGeneric =
    fieldName match {
      case "multiplier" => this.copy(multiplier = newValue.asInstanceOf[Double])
      case "att"        => this.copy(att = newValue.asInstanceOf[Attribute])
      case "turns"      => this.copy(turns = newValue.asInstanceOf[Int])
    }
}

case class SpikeType(
    multiplier: Double,
    cardType: CardType,
    turns: Int
) extends SkillEffect(
      s"${multiplier}x ATK for $cardType type for $turns turns. "
    ) {
  override def getFieldTypes: List[SkillEffectFieldType] =
    Util.getFieldTypes[SpikeType]
  override def withNewField(
      fieldName: String,
      newValue: Any
  ): SkillEffectGeneric =
    fieldName match {
      case "multiplier" => this.copy(multiplier = newValue.asInstanceOf[Double])
      case "cardType"   => this.copy(cardType = newValue.asInstanceOf[CardType])
      case "turns"      => this.copy(turns = newValue.asInstanceOf[Int])
    }
}

sealed trait SpikeScaling extends SkillEffectGeneric {
  val turns: Int
}

case class SpikeScalingGeneric(turns: Int)
    extends SpikeScaling
    with SkillEffectGeneric {
  override def getFieldTypes: List[SkillEffectFieldType] = List(
    SkillEffectFieldType.INT
  )
  override def withNewField(
      fieldName: String,
      newValue: Any
  ): SkillEffectGeneric = {
    fieldName match {
      case "turns" => this.copy(turns = newValue.asInstanceOf[Int])
    }
  }
  override def canMatch(that: SkillEffectGeneric): Boolean = {
    val thatClass = that.getClass
    thatClass == classOf[SpikeScalingByAwakening]
    || thatClass == classOf[SpikeScalingByAttributeAndType]
  }
}

case class SpikeScalingByAwakening(
    dmgScaling: Double,
    awks: List[Awakening],
    turns: Int
) extends SpikeScaling
    with SkillEffect(
      s"1+(${dmgScaling}x) ATK for each ${awks.mkString(", ")} awakening on the team for $turns turns. "
    ) {
  override def getFieldTypes: List[SkillEffectFieldType] =
    Util.getFieldTypes[SpikeScalingByAwakening]
  override def withNewField(
      fieldName: String,
      newValue: Any
  ): SkillEffectGeneric =
    fieldName match {
      case "dmgScaling" => this.copy(dmgScaling = newValue.asInstanceOf[Double])
      case "awks"  => this.copy(awks = newValue.asInstanceOf[List[Awakening]])
      case "turns" => this.copy(turns = newValue.asInstanceOf[Int])
    }
}

case class SpikeScalingByAttributeAndType(
    dmgScaling: Double,
    atts: List[Attribute],
    types: List[CardType],
    turns: Int
) extends SpikeScaling
    with SkillEffect(
      s"1+(${dmgScaling}x) ATK for each ${(atts ++ types).mkString(", ")} card on the team for $turns turns. "
    ) {
  override def getFieldTypes: List[SkillEffectFieldType] =
    Util.getFieldTypes[SpikeScalingByAttributeAndType]
  override def withNewField(
      fieldName: String,
      newValue: Any
  ): SkillEffectGeneric =
    fieldName match {
      case "dmgScaling" => this.copy(dmgScaling = newValue.asInstanceOf[Double])
      case "atts"  => this.copy(atts = newValue.asInstanceOf[List[Attribute]])
      case "types" => this.copy(types = newValue.asInstanceOf[List[CardType]])
      case "turns" => this.copy(turns = newValue.asInstanceOf[Int])
    }
}

case class SpikeSlots(multiplier: Double, slots: List[CardSlot], turns: Int)
    extends SkillEffect(
      s"${multiplier}x ATK for ${slots.mkString(", ")} for $turns turns. "
    ) {
  override def getFieldTypes: List[SkillEffectFieldType] =
    Util.getFieldTypes[SpikeSlots]
  override def withNewField(
      fieldName: String,
      newValue: Any
  ): SkillEffectGeneric =
    fieldName match {
      case "multiplier" => this.copy(multiplier = newValue.asInstanceOf[Double])
      case "slots" => this.copy(slots = newValue.asInstanceOf[List[CardSlot]])
      case "turns" => this.copy(turns = newValue.asInstanceOf[Int])
    }
}

sealed trait Spinner {
  val numSpinners: Int
  val speed: Double
  val turns: Int
}

case class SpinnerGeneric(numSpinners: Int, speed: Double, turns: Int)
    extends Spinner
    with SkillEffectGeneric {
  override def getFieldTypes: List[SkillEffectFieldType] = List(
    SkillEffectFieldType.INT,
    SkillEffectFieldType.DOUBLE,
    SkillEffectFieldType.INT
  )
  override def withNewField(
      fieldName: String,
      newValue: Any
  ): SkillEffectGeneric = {
    fieldName match {
      case "numSpinners" => this.copy(numSpinners = newValue.asInstanceOf[Int])
      case "speed"       => this.copy(speed = newValue.asInstanceOf[Double])
      case "turns"       => this.copy(turns = newValue.asInstanceOf[Int])
    }
  }
  override def canMatch(that: SkillEffectGeneric): Boolean = {
    val thatClass = that.getClass
    thatClass == classOf[SpinnerRandom] || thatClass == classOf[SpinnerFixed]
  }
}

case class SpinnerRandom(numSpinners: Int, speed: Double, turns: Int)
    extends Spinner
    with SkillEffect(
      s"Spawn $numSpinners spinners at random at $speed for $turns turns. "
    ) {
  override def getFieldTypes: List[SkillEffectFieldType] =
    Util.getFieldTypes[SpinnerRandom]
  override def withNewField(
      fieldName: String,
      newValue: Any
  ): SkillEffectGeneric =
    fieldName match {
      case "numSpinners" => this.copy(numSpinners = newValue.asInstanceOf[Int])
      case "speed"       => this.copy(speed = newValue.asInstanceOf[Double])
      case "turns"       => this.copy(turns = newValue.asInstanceOf[Int])
    }
}

case class SpinnerFixed(
    positions: BoardPositions,
    speed: Double,
    turns: Int,
    numSpinners: Int
) extends Spinner
    with SkillEffect(
      s"Spawn spinners in the following positions: \n${Board
          .boardPositionsToCompleteStr(positions)} \nat $speed speed for $turns turns."
    ) {
  override def getFieldTypes: List[SkillEffectFieldType] =
    Util.getFieldTypes[SpinnerFixed]
  override def withNewField(
      fieldName: String,
      newValue: Any
  ): SkillEffectGeneric =
    fieldName match {
      case "positions" =>
        this.copy(positions = newValue.asInstanceOf[BoardPositions])
      case "speed"       => this.copy(speed = newValue.asInstanceOf[Double])
      case "turns"       => this.copy(turns = newValue.asInstanceOf[Int])
      case "numSpinners" => this.copy(numSpinners = newValue.asInstanceOf[Int])
    }
}

object SpinnerFixed {
  def apply(positions: BoardPositions, speed: Double, turns: Int) = {
    new SpinnerFixed(positions, speed, turns, positions.countPositions)
  }
}

case class Suicide(percentLost: Double)
    extends SkillEffect(s"HP reduced by ${percentLost}%. ") {
  override def getFieldTypes: List[SkillEffectFieldType] =
    Util.getFieldTypes[Suicide]
  override def withNewField(
      fieldName: String,
      newValue: Any
  ): SkillEffectGeneric =
    fieldName match {
      case "percentLost" =>
        this.copy(percentLost = newValue.asInstanceOf[Double])
    }
}
sealed trait TimeExtendSkill {
  val turns: Int
}

case class TimeExtendSkillGeneric(turns: Int)
    extends TimeExtendSkill
    with SkillEffectGeneric {
  override def getFieldTypes: List[SkillEffectFieldType] = List(
    SkillEffectFieldType.INT
  )
  override def withNewField(
      fieldName: String,
      newValue: Any
  ): SkillEffectGeneric = {
    fieldName match {
      case "turns" => this.copy(turns = newValue.asInstanceOf[Int])
    }
  }
  override def canMatch(that: SkillEffectGeneric): Boolean = {
    val thatClass = that.getClass
    thatClass == classOf[TimeExtendFlat] || thatClass == classOf[TimeExtendMult]
  }
}

case class TimeExtendFlat(seconds: Int, turns: Int)
    extends TimeExtendSkill
    with SkillEffect(
      s"${if (seconds > 0) "Extends" else "Reduces"} move time by $seconds seconds for $turns turns. "
    ) {
  override def getFieldTypes: List[SkillEffectFieldType] =
    Util.getFieldTypes[TimeExtendFlat]
  override def withNewField(
      fieldName: String,
      newValue: Any
  ): SkillEffectGeneric =
    fieldName match {
      case "seconds" => this.copy(seconds = newValue.asInstanceOf[Int])
      case "turns"   => this.copy(turns = newValue.asInstanceOf[Int])
    }
}

case class TimeExtendMult(mult: Double, turns: Int)
    extends TimeExtendSkill
    with SkillEffect(
      s"${mult}x move time for $turns turns. "
    ) {
  override def getFieldTypes: List[SkillEffectFieldType] =
    Util.getFieldTypes[TimeExtendMult]
  override def withNewField(
      fieldName: String,
      newValue: Any
  ): SkillEffectGeneric =
    fieldName match {
      case "mult"  => this.copy(mult = newValue.asInstanceOf[Int])
      case "turns" => this.copy(turns = newValue.asInstanceOf[Int])
    }
}

sealed trait Transform {}

case class TransformGeneric() extends Transform with SkillEffect("Transform") {
  override def getFieldTypes: List[SkillEffectFieldType] = List()
  override def withNewField(
      fieldName: String,
      newValue: Any
  ): SkillEffectGeneric = ???
  override def canMatch(that: SkillEffectGeneric): Boolean = {
    val thatClass = that.getClass
    thatClass == classOf[TransformFixed]
    || thatClass == classOf[TransformRandom]
  }
}

case class TransformFixed(
    targetId: Int,
    targetName: String
) extends Transform
    with SkillEffect(s"Transform into #$targetId - $targetName") {

  override def getFieldTypes: List[SkillEffectFieldType] =
    Util.getFieldTypes[TransformFixed]
  override def withNewField(
      fieldName: String,
      newValue: Any
  ): SkillEffectGeneric =
    fieldName match {
      case "targetId"   => this.copy(targetId = newValue.asInstanceOf[Int])
      case "targetName" => this.copy(targetName = newValue.asInstanceOf[String])
    }
}

case class TransformRandom(targets: List[(Int, String)])
    extends Transform
    with SkillEffect(
      s"Transforms randomly into one of the following: \n${targets.zipWithIndex
          .map((targ, ind) => s"${ind + 1}. ${targ._1} - ${targ._2}")
          .mkString("\n")}"
    ) {
  override def getFieldTypes: List[SkillEffectFieldType] =
    Util.getFieldTypes[TransformRandom]
  override def withNewField(
      fieldName: String,
      newValue: Any
  ): SkillEffectGeneric =
    fieldName match {
      case "targets" =>
        this.copy(targets = newValue.asInstanceOf[List[(Int, String)]])
    }
}

case class UnableToUseSkills(turns: Int)
    extends SkillEffect(
      s"Unable to use skills for $turns turns. "
    ) {

  override def getFieldTypes: List[SkillEffectFieldType] =
    Util.getFieldTypes[UnableToUseSkills]
  override def withNewField(
      fieldName: String,
      newValue: Any
  ): SkillEffectGeneric =
    fieldName match {
      case "turns" => this.copy(turns = newValue.asInstanceOf[Int])
    }
}

case class UnlockOrbs() extends SkillEffect("Unlock all orbs. ") {
  override def getFieldTypes: List[SkillEffectFieldType] =
    Util.getFieldTypes[UnlockOrbs]
  override def withNewField(
      fieldName: String,
      newValue: Any
  ): SkillEffectGeneric =
    ???
}

case class UnmatchableClear(turns: Int)
    extends SkillEffect(
      s"Unmatchable reduced status by $turns turns. "
    ) {
  override def getFieldTypes: List[SkillEffectFieldType] =
    Util.getFieldTypes[UnmatchableClear]
  override def withNewField(
      fieldName: String,
      newValue: Any
  ): SkillEffectGeneric =
    fieldName match {
      case "turns" => this.copy(turns = newValue.asInstanceOf[Int])
    }
}
sealed trait Void {}

case class VoidDamageAbsorb(turns: Int)
    extends Void
    with SkillEffect(
      s"Voids damage absorption for $turns turns. "
    ) {
  override def getFieldTypes: List[SkillEffectFieldType] =
    Util.getFieldTypes[VoidDamageAbsorb]
  override def withNewField(
      fieldName: String,
      newValue: Any
  ): SkillEffectGeneric =
    fieldName match {
      case "turns" => this.copy(turns = newValue.asInstanceOf[Int])
    }
}

case class VoidAttributeAbsorb(turns: Int)
    extends Void
    with SkillEffect(
      s"Voids attribute absorption for $turns turns. "
    ) {
  override def getFieldTypes: List[SkillEffectFieldType] =
    Util.getFieldTypes[VoidAttributeAbsorb]
  override def withNewField(
      fieldName: String,
      newValue: Any
  ): SkillEffectGeneric =
    fieldName match {
      case "turns" => this.copy(turns = newValue.asInstanceOf[Int])
    }
}

case class VoidVoid(turns: Int)
    extends Void
    with SkillEffect(
      s"Voids damage void for $turns turns. "
    ) {
  override def getFieldTypes: List[SkillEffectFieldType] =
    Util.getFieldTypes[VoidVoid]
  override def withNewField(
      fieldName: String,
      newValue: Any
  ): SkillEffectGeneric =
    fieldName match {
      case "turns" => this.copy(turns = newValue.asInstanceOf[Int])
    }
}

case class TimeReducedForTop2(args: List[Int])
    extends SkillEffect(
      s"Orb move time halved for 1 turn for the top 2 ranked players"
    ) {
  override def getFieldTypes: List[SkillEffectFieldType] =
    Util.getFieldTypes[TimeReducedForTop2]
  override def withNewField(
      fieldName: String,
      newValue: Any
  ): SkillEffectGeneric =
    fieldName match {
      case "args" => this.copy(args = newValue.asInstanceOf[List[Int]])
    }
}
