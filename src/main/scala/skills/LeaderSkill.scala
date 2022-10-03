package skills

import skills.effects.leader._
import model._

case class LeaderSkill(name: String, effect: LSEffect) {}

object LeaderSkill {
  val NO_LS = LeaderSkill("None", LSEffectNone)

  def fromJson(
      jsonId: Int,
      skillData: Seq[JsonSkillData],
      cardData: Seq[JsonCardData]
  ): LeaderSkill = {
    if (jsonId == 0) NO_LS
    else {
      val jsd = skillData(jsonId)
      effectsFromJson(jsd, skillData, cardData)
    }
  }

  def effectsFromJson(
      jsd: JsonSkillData,
      skillData: Seq[JsonSkillData],
      cardData: Seq[JsonCardData]
  ): LeaderSkill = {
    val args = new SafeList(jsd.args)
    val effect: LSEffect = jsd.internalEffectId match {
      case 3 =>
        LSEffectNone // wtf this is an active skill, why does 207934 have this as a LS????
      case 11 =>
        LSComponent(
          ConditionAttribute(Attribute.from(args(0))),
          ATKBoost(mult = args(1) / 100.0)
        )
      case 12 =>
        LSComponent(
          ConditionMatchingAnyOrbs,
          BonusAttackScaling(ratio = args(0) / 100.0)
        )
      case 13 =>
        LSComponent(
          ConditionMatchingAnyOrbs,
          AutoHealScaling(ratio = args(0) / 100.0)
        )
      case 14 =>
        LSComponent(
          ConditionHPThreshold(
            hpPercent = args(0),
            isGreater = true,
            allowEqual = true
          ),
          Resolve
        )
      case 15 => LSPassive(TimeExtend(seconds = args(0) / 100.0))
      case 16 => LSPassive(ShieldRegular(args(0)))
      case 17 =>
        LSPassive(
          ShieldElement(reduction = args(1), att = Attribute.from(args(0)))
        )
      case 22 =>
        LSComponent(
          ConditionType(CardType.from(args(0))),
          ATKBoost(args(1) / 100.0)
        )
      case 23 =>
        LSComponent(
          ConditionType(CardType.from(args(0))),
          HPBoost(args(1) / 100.0)
        )
      case 24 =>
        LSComponent(
          ConditionType(CardType.from(args(0))),
          RCVBoost(args(1) / 100.0)
        )
      case 28 =>
        List(ATKBoost(_), RCVBoost(_))
          .map(_(args(1) / 100.0))
          .map(payoff =>
            LSComponent(ConditionAttribute(Attribute.from(args(0))), payoff)
          )
          .reduce(_ and _)
      case 29 =>
        List(HPBoost(_), ATKBoost(_), RCVBoost(_))
          .map(_(args(1) / 100.0))
          .map(payoff =>
            LSComponent(ConditionAttribute(Attribute.from(args(0))), payoff)
          )
          .reduce(_ and _)
      case 30 =>
        List(args(0), args(1))
          .map(CardType.from)
          .map(ct => LSComponent(ConditionType(ct), HPBoost(args(2) / 100.0)))
          .reduce(_ and _)
      case 31 =>
        List(args(0), args(1))
          .map(CardType.from)
          .map(ct => LSComponent(ConditionType(ct), ATKBoost(args(2) / 100.0)))
          .reduce(_ and _)
      case 33 =>
        LSPassive(DrummingSound)
      case 36 =>
        List(args(0), args(1))
          .map(Attribute.from)
          .map(att => LSPassive(ShieldElement(reduction = args(2), att)))
          .reduce(_ and _)
      case 38 => {
        val threshold = args(0)
        val condition =
          if (threshold == 100)
            ConditionHPThreshold(100, isGreater = true, allowEqual = true)
          else
            ConditionHPThreshold(
              threshold,
              isGreater = false,
              allowEqual = true
            )
        LSComponent(condition, ShieldRegular(args(2)))
      }
      case 39 =>
        List(ATKBoost(_), RCVBoost(_))
          .zip(args.slice(1, 3))
          .filter(_._2 != 0)
          .map(_._1(args(3) / 100.0))
          .map(payoff =>
            LSComponent(
              ConditionHPThreshold(
                args(0),
                isGreater = false,
                allowEqual = true
              ),
              payoff
            ): LSEffect
          )
          .reduceOption(_ and _)
          .getOrElse(LSEffectNone)
      case 40 =>
        args
          .slice(0, 2)
          .map(Attribute.from)
          .map(ConditionAttribute(_))
          .map(cond => LSComponent(cond, ATKBoost(args(2) / 100.0)))
          .reduce(_ and _)
      case 41 =>
        LSPassive(
          Chance(
            chance = args(0),
            CounterAttack(
              mult = args(1) / 100.0,
              att = Attribute.from(args(2))
            )
          )
        )
      case 43 =>
        LSComponent(
          ConditionHPThreshold(
            hpPercent = args(0),
            isGreater = true,
            allowEqual = true
          ),
          Chance(args(1), ShieldRegular(args(2)))
        )
      case 44 =>
        (
          List(ATKBoost(_), RCVBoost(_)),
          args.slice(1, 3),
          List(1, 2)
        ).zipped.toList
          .filter(t => t._2 == t._3)
          .map(_._1(args(3) / 100.0))
          .map(payoff =>
            LSComponent(
              ConditionHPThreshold(
                hpPercent = args(0),
                isGreater = true,
                allowEqual = true
              ),
              payoff
            ): LSEffect
          )
          .reduceOption(_ and _)
          .getOrElse(LSEffectNone)
      case 45 =>
        List(HPBoost(_), ATKBoost(_))
          .map(_(args(1) / 100.0))
          .map(payoff =>
            LSComponent(ConditionAttribute(Attribute.from(args(0))), payoff)
          )
          .reduce(_ and _)
      case 46 =>
        args
          .slice(0, 2)
          .map(Attribute.from)
          .map(ConditionAttribute(_))
          .map(cond => LSComponent(cond, HPBoost(args(2) / 100.0)))
          .reduce(_ and _)
      case 48 =>
        LSComponent(
          ConditionAttribute(Attribute.from(args(0))),
          HPBoost(args(1) / 100.0)
        )
      case 49 =>
        LSComponent(
          ConditionAttribute(Attribute.from(args(0))),
          RCVBoost(args(1) / 100.0)
        )
      case 50 => LSEffectNone // active skill, card 207907
      case 54 => LSPassive(CoinBoost(args(0) / 100.0))
      case 138 => {
        args
          .map(jsdId =>
            effectsFromJson(skillData(jsdId), skillData, cardData).effect
          )
          .reduceRight(_ and _)
      }
      case 185 => {
        val time = args(0) / 100.0
        val atts = Attribute.fromBitFlag(args(1))
        val types = CardType.fromBitFlag(args(2))
        val hpmul = args(3) / 100.0
        val atkmul = args(4) / 100.0
        val rcvmul = args(5) / 100.0
        val payoffs =
          List(hpmul, atkmul, rcvmul)
            .zip(
              List(HPBoost(_), ATKBoost(_), RCVBoost(_))
            )
            .filter(_._1 > 0)
            .map((mul, pay) => pay(mul))
        val conditions =
          atts.map(ConditionAttribute(_)) ++ types.map(ConditionType(_))

        payoffs
          .flatMap(payoff =>
            conditions.map(cond => LSComponent(cond, payoff): LSEffect)
          )
          .reduce(_ and _) and (
          if (time > 0) LSPassive(TimeExtend(time)) else LSEffectNone
        )
      }
      case n => {
        println()
        println(s"!!!!!Skill id $n not implemented.")
        println(s"args: $args")
        println("desc: ")
        println(jsd.desc)
        ???
      }
    }
    LeaderSkill(jsd.name, effect)
  }
}
