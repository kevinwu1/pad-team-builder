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
      case 61 => {
        val atts = Attribute.fromBitFlag(args(0))
        val minCount = args(1)
        val baseMult = args(2) / 100.0
        val scaleMult = args(3) / 100.0
        val maxExtra = Math.min(args(4), atts.size - minCount)
        val effects = (0 to maxExtra).reverse
          .map(extraMatches => {
            val mult = baseMult + scaleMult * extraMatches
            val matches = minCount + extraMatches
            LSComponent(
              ConditionColorsMatched(atts = atts, matchRequirement = matches),
              ATKBoost(mult)
            )
          })
          .toList
        firstOf(effects)
      }
      case 62 =>
        List(HPBoost(_), ATKBoost(_))
          .map(_(args(1) / 100.0))
          .map(payoff =>
            LSComponent(ConditionType(CardType.from(args(0))), payoff)
          )
          .reduce(_ and _)
      case 63 =>
        List(HPBoost(_), RCVBoost(_))
          .map(_(args(1) / 100.0))
          .map(payoff =>
            LSComponent(ConditionType(CardType.from(args(0))), payoff)
          )
          .reduce(_ and _)
      case 64 =>
        List(ATKBoost(_), RCVBoost(_))
          .map(_(args(1) / 100.0))
          .map(payoff =>
            LSComponent(ConditionType(CardType.from(args(0))), payoff)
          )
          .reduce(_ and _)
      case 65 =>
        List(HPBoost(_), ATKBoost(_), RCVBoost(_))
          .map(_(args(1) / 100.0))
          .map(payoff =>
            LSComponent(ConditionType(CardType.from(args(0))), payoff)
          )
          .reduce(_ and _)
      case 66 =>
        LSComponent(ConditionCombos(args(0)), ATKBoost(args(1) / 100.0))
      case 67 =>
        List(HPBoost(_), RCVBoost(_))
          .map(_(args(1) / 100.0))
          .map(payoff =>
            LSComponent(ConditionAttribute(Attribute.from(args(0))), payoff)
          )
          .reduce(_ and _)
      case 69 =>
        List(
          ConditionAttribute(Attribute.from(args(0))),
          ConditionType(CardType.from(args(1)))
        ).map(cond =>
          LSComponent(
            cond,
            ATKBoost(args(2) / 100.0)
          )
        ).reduce(_ and _)
      case 75 =>
        List(
          ConditionAttribute(Attribute.from(args(0))),
          ConditionType(CardType.from(args(1)))
        ).flatMap(cond =>
          List(ATKBoost(_), RCVBoost(_))
            .map(_(args(2) / 100.0))
            .map(payoff => LSComponent(cond, payoff))
        ).reduce(_ and _)
      case 76 =>
        List(
          ConditionAttribute(Attribute.from(args(0))),
          ConditionType(CardType.from(args(1)))
        ).flatMap(cond =>
          List(HPBoost(_), ATKBoost(_), RCVBoost(_))
            .map(_(args(2) / 100.0))
            .map(payoff => LSComponent(cond, payoff))
        ).reduce(_ and _)
      case 77 =>
        args
          .slice(0, 2)
          .map(CardType.from)
          .map(ConditionType(_))
          .flatMap(cond =>
            List(HPBoost(_), ATKBoost(_))
              .map(_(args(2) / 100.0))
              .map(payoff => LSComponent(cond, payoff))
          )
          .reduce(_ and _)
      case 79 =>
        args
          .slice(0, 2)
          .map(CardType.from)
          .map(ConditionType(_))
          .flatMap(cond =>
            List(ATKBoost(_), RCVBoost(_))
              .map(_(args(2) / 100.0))
              .map(payoff => LSComponent(cond, payoff))
          )
          .reduce(_ and _)
      case 90 => LSEffectNone // Active skill, 207892
      case 94 =>
        (
          List(ATKBoost(_), RCVBoost(_)),
          args.slice(2, 4),
          List(1, 2)
        ).zipped.toList
          .filter(t => t._2 == t._3)
          .map(_._1(args(4) / 100.0))
          .map(payoff =>
            LSComponent(
              ConditionHPThreshold(
                hpPercent = args(0),
                isGreater = false,
                allowEqual = true
              ) and ConditionAttribute(Attribute.from(args(1))),
              payoff
            ): LSEffect
          )
          .reduceOption(_ and _)
          .getOrElse(LSEffectNone)
      case 95 =>
        (
          List(ATKBoost(_), RCVBoost(_)),
          args.slice(2, 4),
          List(1, 2)
        ).zipped.toList
          .filter(t => t._2 == t._3)
          .map(_._1(args(4) / 100.0))
          .map(payoff =>
            LSComponent(
              ConditionHPThreshold(
                hpPercent = args(0),
                isGreater = false,
                allowEqual = true
              ) and ConditionType(CardType.from(args(1))),
              payoff
            ): LSEffect
          )
          .reduceOption(_ and _)
          .getOrElse(LSEffectNone)
      case 96 =>
        (
          List(ATKBoost(_), RCVBoost(_)),
          args.slice(2, 4),
          List(1, 2)
        ).zipped.toList
          .filter(t => t._2 == t._3)
          .map(_._1(args(4) / 100.0))
          .map(payoff =>
            LSComponent(
              ConditionHPThreshold(
                hpPercent = args(0),
                isGreater = true,
                allowEqual = true
              ) and ConditionAttribute(Attribute.from(args(1))),
              payoff
            ): LSEffect
          )
          .reduceOption(_ and _)
          .getOrElse(LSEffectNone)
      case 97 =>
        (
          List(ATKBoost(_), RCVBoost(_)),
          args.slice(2, 4),
          List(1, 2)
        ).zipped.toList
          .filter(t => t._2 == t._3)
          .map(_._1(args(4) / 100.0))
          .map(payoff =>
            LSComponent(
              ConditionHPThreshold(
                hpPercent = args(0),
                isGreater = true,
                allowEqual = true
              ) and ConditionType(CardType.from(args(1))),
              payoff
            ): LSEffect
          )
          .reduceOption(_ and _)
          .getOrElse(LSEffectNone)
      case 98 => {
        val minCombo = args(0)
        val baseMult = args(1) / 100.0
        val scaleMult = args(2) / 100.0
        val maxCombo = args(3)
        val effects = (minCombo to maxCombo).reverse
          .map(combos => {
            val extraCombos = combos - minCombo
            val mult = baseMult + scaleMult * extraCombos
            LSComponent(
              ConditionCombos(numCombos = combos),
              ATKBoost(mult)
            )
          })
          .toList
        firstOf(effects)
      }
      case 100 =>
        (
          List(ATKBoost(_), RCVBoost(_)),
          args.slice(0, 2),
          List(1, 2)
        ).zipped.toList
          .filter(t => t._2 == t._3)
          .map(_._1(args(2) / 100.0))
          .map(payoff =>
            LSComponent(
              SkillUsed,
              payoff
            ): LSEffect
          )
          .reduceOption(_ and _)
          .getOrElse(LSEffectNone)
      case 101 =>
        LSComponent(ConditionCombosExact(args(0)), ATKBoost(args(1) / 100.0))
      case 104 =>
        (
          List(ATKBoost(_), RCVBoost(_)),
          args.slice(2, 4),
          List(1, 2)
        ).zipped.toList
          .filter(t => t._2 == t._3)
          .map(_._1(args(4) / 100.0))
          .flatMap(payoff =>
            Attribute
              .fromBitFlag(args(1))
              .map(ConditionAttribute(_))
              .map(cond =>
                LSComponent(
                  cond and ConditionCombos(args(0)),
                  payoff
                ): LSEffect
              )
          )
          .reduceOption(_ and _)
          .getOrElse(LSEffectNone)
      case 105 =>
        LSPassive(RCVBoost(args(0) / 100.0)) and LSPassive(
          ATKBoost(args(1) / 100.0)
        )
      case 108 =>
        LSPassive(HPBoost(args(0) / 100.0)) and
          LSComponent(
            ConditionType(CardType.from(args(1))),
            ATKBoost(args(2) / 100.0)
          )
      case 111 =>
        args
          .slice(0, 2)
          .map(Attribute.from)
          .map(ConditionAttribute(_))
          .flatMap(cond =>
            List(HPBoost(_), ATKBoost(_))
              .map(_(args(2) / 100.0))
              .map(payoff => LSComponent(cond, payoff))
          )
          .reduce(_ and _)
      case 114 =>
        args
          .slice(0, 2)
          .map(Attribute.from)
          .map(ConditionAttribute(_))
          .flatMap(cond =>
            List(HPBoost(_), ATKBoost(_), RCVBoost(_))
              .map(_(args(2) / 100.0))
              .map(payoff => LSComponent(cond, payoff))
          )
          .reduce(_ and _)
      case 115 => LSEffectNone
      case 116 => LSEffectNone
      case 119 => {
        val atts = Attribute.fromBitFlag(args(0))
        val minMatch = args(1)
        val baseMult = args(2) / 100.0
        val scaleMult = args(3) / 100.0
        val maxMatch = Math.max(args(4), minMatch)
        val effects = (minMatch to maxMatch).reverse
          .flatMap(orbs => {
            val extraorbs = orbs - minMatch
            val mult = baseMult + scaleMult * extraorbs
            atts.map(att =>
              LSComponent(
                ConditionOrbsLinked(numOrbs = orbs, att = att),
                ATKBoost(mult)
              )
            )
          })
          .toList
        firstOf(effects)
      }
      case 121 =>
        (Attribute.fromBitFlag(args(0)).map(ConditionAttribute(_)) ++
          CardType.fromBitFlag(args(1)).map(ConditionType(_)))
          .flatMap(cond =>
            List(HPBoost(args(2) / 100.0), ATKBoost(args(3) / 100.0)).map(
              payoff =>
                LSComponent(
                  cond,
                  payoff
                )
            )
          )
          .reduce(_ and _)
      case 122 =>
        (Attribute.fromBitFlag(args(1)).map(ConditionAttribute(_)) ++
          CardType.fromBitFlag(args(2)).map(ConditionType(_)))
          .flatMap(cond =>
            List(ATKBoost(_), RCVBoost(_))
              .zip(args.slice(3, 5).map(_ / 100.0))
              .filter(_._2 != 0)
              .map(_(_))
              .map(payoff =>
                LSComponent(
                  cond and ConditionHPThreshold(
                    args(0),
                    isGreater = false,
                    allowEqual = true
                  ),
                  payoff
                ): LSEffect
              )
          )
          .reduceOption(_ and _)
          .getOrElse(LSEffectNone)
      case 123 =>
        (Attribute.fromBitFlag(args(1)).map(ConditionAttribute(_)) ++
          CardType.fromBitFlag(args(2)).map(ConditionType(_)))
          .map(cond =>
            LSComponent(
              cond and ConditionHPThreshold(
                args(0),
                isGreater = true,
                allowEqual = true
              ),
              ATKBoost(args(3) / 100.0)
            ): LSEffect
          )
          .reduceOption(_ and _)
          .getOrElse(LSEffectNone)
      case 124 => {
        val atts_1 =
          args.slice(0, 5).filter(_ != 0).map(Attribute.fromBitFlag(_))
        if (atts_1.exists(_.size != 1)) {
          println("NOT EXPECTED !!!!")
          println(args)
          println(jsd)
          ???
        }
        val atts = atts_1.map(_.head)
        val minCount = args(5)
        val baseMult = args(6) / 100.0
        val scaleMult = args(7) / 100.0
        val maxExtra = atts.size - minCount
        val effects = (0 to maxExtra).reverse
          .map(extraMatches => {
            val mult = baseMult + scaleMult * extraMatches
            val matches = minCount + extraMatches
            LSComponent(
              ConditionColorsMatched(atts = atts, matchRequirement = matches),
              ATKBoost(mult)
            )
          })
          .toList
        firstOf(effects)
      }
      case 125 =>
        List(HPBoost(_), ATKBoost(_), RCVBoost(_))
          .zip(args.slice(5, 8).map(_ / 100.0))
          .filter(_._2 != 0)
          .map(_(_))
          .map(payoff =>
            LSComponent(
              args
                .slice(0, 5)
                .filter(_ != 0)
                .map(ConditionCardOnTeam(_): LSCondition)
                .reduce(_ and _),
              payoff
            )
          )
          .reduce(_ and _)
      case 126 => LSEffectNone
      case 128 => LSEffectNone
      case 129 => {
        if (args.arr == List(8, 0, 100)) LSEffectNone
        else {
          val atts = Attribute.fromBitFlag(args(0))
          val types = CardType.fromBitFlag(args(1))
          val hpmul = args(2) / 100.0
          val atkmul = args(3) / 100.0
          val rcvmul = args(4) / 100.0
          val resistAtts = Attribute.fromBitFlag(args(5))
          val resistance = args(6)
          val payoffs =
            List(HPBoost(_), ATKBoost(_), RCVBoost(_))
              .zip(List(hpmul, atkmul, rcvmul))
              .filter(_._2 > 0)
              .map(_(_))
          val conditions =
            atts.map(ConditionAttribute(_)) ++ types.map(ConditionType(_))

          (payoffs
            .flatMap(payoff =>
              conditions.map(cond => LSComponent(cond, payoff): LSEffect)
            ) ++ resistAtts
            .map(att => LSPassive(ShieldElement(resistance, att))))
            .reduce(_ and _)
        }
      }
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
        println(s"!!!!!!!!!!!!!!LS id $n not implemented.")
        println(s"args: $args")
        println("desc: ")
        println(jsd.desc)
        ???
      }
    }
    LeaderSkill(jsd.name, effect)
  }
}
