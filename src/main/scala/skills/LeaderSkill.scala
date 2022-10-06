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
      case 73 =>
        List(HPBoost(_), ATKBoost(_))
          .zip(List.fill(2)(args(2)))
          .filter(_._2 != 0)
          .map(_(_))
          .map(payoff =>
            LSComponent(
              ConditionAttribute(Attribute.from(args(0))) and ConditionType(
                CardType.from(args(1))
              ),
              payoff
            )
          )
          .reduce(_ and _)
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
              ConditionSkillUsed,
              payoff
            ): LSEffect
          )
          .reduceOption(_ and _)
          .getOrElse(LSEffectNone)
      case 101 =>
        LSComponent(ConditionCombosExact(args(0)), ATKBoost(args(1) / 100.0))
      case 103 => {
        (
          List(HPBoost(_), RCVBoost(_)),
          args.slice(1, 3),
          List(1, 2)
        ).zipped.toList
          .filter(t => t._2 == t._3)
          .map(_._1(args(3)))
          .map(LSComponent(ConditionCombos(args(0)), _))
          .reduce(_ and _)
      }
      case 104 =>
        (
          List(ATKBoost(_), RCVBoost(_)),
          args.slice(2, 4),
          List(1, 2)
        ).zipped.toList
          .filter(t => t._2 == t._3)
          .map(_._1(args(4) / 100.0))
          .flatMap(payoff =>
            conditionAttributesFromBits(args(1))
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
      case 107 =>
        LSPassive(HPBoost(args(0) / 100.0)) and
          (if (args(1) != 0) {
             conditionAttributesFromBits(args(1))
               .map(cond => LSComponent(cond, ATKBoost(args(2))))
               .reduce(_ and _)
           } else LSEffectNone)
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
        val conditions = (conditionAttributesFromBits(args(0)) ++
          conditionTypesFromBits(args(1)))
        val payoffs = List(HPBoost(_), ATKBoost(_), RCVBoost(_))
          .zip(args.slice(2, 5).map(_ / 100.0))
          .filter(_._2 != 0)
          .map(_(_))
        conditions
          .cartesian(payoffs)
          .map(LSComponent(_, _): LSEffect)
          .reduceOption(_ and _)
          .getOrElse(LSEffectNone)
      case 122 =>
        (conditionAttributesFromBits(args(1)) ++
          conditionTypesFromBits(args(2)))
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
        (conditionAttributesFromBits(args(1)) ++
          conditionTypesFromBits(args(2)))
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
          val atts = conditionAttributesFromBits(args(0))
          val types = conditionTypesFromBits(args(1))
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
          val conditions = atts ++ types

          (payoffs
            .flatMap(payoff =>
              conditions.map(cond => LSComponent(cond, payoff): LSEffect)
            ) ++ resistAtts
            .map(att => LSPassive(ShieldElement(resistance, att))))
            .reduce(_ and _)
        }
      }
      case 130 => {
        val threshold = args(0)
        val atts = conditionAttributesFromBits(args(1))
        val types = conditionTypesFromBits(args(2))
        val atkmul = args(3) / 100.0
        val rcvmul = args(4) / 100.0
        val resistAtts = Attribute.fromBitFlag(args(5))
        val resistance = args(6)
        val payoffs =
          List(ATKBoost(_), RCVBoost(_))
            .zip(List(atkmul, rcvmul))
            .filter(_._2 > 0)
            .map(_(_))
        val conditions = atts ++ types

        (payoffs ++ resistAtts.map(att => ShieldElement(resistance, att)))
          .flatMap(payoff =>
            conditions.map(cond =>
              LSComponent(
                ConditionHPThreshold(
                  threshold,
                  isGreater = false,
                  allowEqual = true
                ) and cond,
                payoff
              ): LSEffect
            )
          )
          .reduceOption(_ and _)
          .getOrElse(LSEffectNone)
      }
      case 131 => {
        val threshold = args(0)
        val atts = conditionAttributesFromBits(args(1))
        val types = conditionTypesFromBits(args(2))
        val atkmul = args(3) / 100.0
        val rcvmul = args(4) / 100.0
        val resistAtts = Attribute.fromBitFlag(args(5))
        val resistance = args(6)
        val payoffs =
          List(ATKBoost(_), RCVBoost(_))
            .zip(List(atkmul, rcvmul))
            .filter(_._2 > 0)
            .map(_(_))
        val conditions = atts ++ types

        (payoffs ++ resistAtts.map(att => ShieldElement(resistance, att)))
          .flatMap(payoff =>
            conditions.map(cond =>
              LSComponent(
                ConditionHPThreshold(
                  threshold,
                  isGreater = true,
                  allowEqual = true
                ) and cond,
                payoff
              ): LSEffect
            )
          )
          .reduce(_ and _)
      }
      case 132 => LSEffectNone
      case 133 => {
        val atts = conditionAttributesFromBits(args(0))
        val types = conditionTypesFromBits(args(1))
        val atkmul = args(2) / 100.0
        val rcvmul = args(3) / 100.0
        val payoffs =
          List(ATKBoost(_), RCVBoost(_))
            .zip(List(atkmul, rcvmul))
            .filter(_._2 > 0)
            .map(_(_))
        val conditions = atts ++ types

        payoffs
          .flatMap(payoff =>
            conditions.map(cond =>
              LSComponent(
                ConditionSkillUsed and cond,
                payoff
              ): LSEffect
            )
          )
          .reduce(_ and _)
      }
      case 136 => {
        val boost1 = args.slice(0, 4)
        val boost2 = args.slice(4, 8)
        List(boost1, boost2)
          .map(boost =>
            val condition =
              ConditionAttribute(Attribute.firstFromBitFlag(boost(0)))
            val payoffs: List[LSPayoff] =
              List(HPBoost(_), ATKBoost(_), RCVBoost(_))
                .zip(boost.slice(1, 4).map(_ / 100.0))
                .filter(_._2 != 0)
                .map(_(_))
            (condition, payoffs)
          )
          .flatMap(effect =>
            effect._2
              .map(payoff => LSComponent(effect._1, payoff))
          )
          .reduce(_ and _)
      }
      case 137 => {
        val boost1 = args.slice(0, 4)
        val boost2 = args.slice(4, 8)
        List(boost1, boost2)
          .map(boost =>
            val condition = ConditionType(CardType.fromBitFlag(boost(0)).head)
            val payoffs: List[LSPayoff] =
              List(HPBoost(_), ATKBoost(_), RCVBoost(_))
                .zip(boost.slice(1, 4).map(_ / 100.0))
                .filter(_._2 != 0)
                .map(_(_))
            (condition, payoffs)
          )
          .flatMap(effect =>
            effect._2
              .map(payoff => LSComponent(effect._1, payoff))
          )
          .reduce(_ and _)
      }
      case 138 => {
        args
          .map(jsdId =>
            effectsFromJson(skillData(jsdId), skillData, cardData).effect
          )
          .reduceRight(_ and _)
      }
      case 139 => {
        val atts = conditionAttributesFromBits(args(0))
        val types = conditionTypesFromBits(args(1))
        val conditions = atts ++ types
        args
          .slice(2, 8)
          .grouped(3)
          .flatMap(pargs =>
            val threshold = pargs(0)
            val isGreater = pargs(1) == 0
            val mult = pargs(2) / 100.0
            conditions.map(cond =>
              LSComponent(
                cond and ConditionHPThreshold(
                  threshold,
                  isGreater = isGreater,
                  allowEqual = true
                ),
                ATKBoost(mult)
              )
            )
          )
          .reduce(_ and _)
      }
      case 141 => LSEffectNone
      case 148 => LSPassive(RankExpBoost(args(0) / 100.0))
      case 150 => LSComponent(ConditionSparkle, ATKBoost(args(1) / 100.0))
      case 151 =>
        List(ATKBoost(_), RCVBoost(_))
          .zip(args.slice(0, 2).map(_ / 100.0))
          .filter(_._2 != 0)
          .map(_(_))
          .:++(if (args(2) != 0) List(ShieldRegular(args(2))) else List())
          .map(LSComponent(ConditionCross(Attribute.HEART), _))
          .reduce(_ and _)
      case 157 =>
        LSComponentInfinite(
          ConditionCross(Attribute.from(args(0))),
          ATKBoost(args(1) / 100.0)
        )
      case 162 => LSPassive(Board7x6)
      case 163 => LSEffectNone
      case 164 => {
        val atts_1 =
          args.slice(0, 4).filter(_ != 0).map(Attribute.fromBitFlag(_))
        val atts =
          if (atts_1.exists(_.size > 1))
            ???
          else atts_1.map(_.head)
        val minMatch = args(4)
        val baseMult = args(5) / 100.0
        val baseRCVMult = args(6) / 100.0
        val scaleMult = args(7) / 100.0
        val maxExtra = atts.size - minMatch
        val effects = (0 to maxExtra).reverse
          .flatMap(extraMatches => {
            val matches = minMatch + extraMatches
            val atkMult = baseMult + scaleMult * extraMatches
            val rcvMult = baseMult + scaleMult * extraMatches
            List(ATKBoost(atkMult), RCVBoost(rcvMult)).map(payoff =>
              LSComponent(
                ConditionColorsMatched(
                  atts = atts,
                  matchRequirement = matches
                ),
                payoff
              )
            )
          })
          .toList
        firstOf(effects)
      }
      case 165 => {
        val atts = Attribute.fromBitFlag(args(0))
        val minCount = args(1)
        val baseMult = args(2) / 100.0
        val baseRcvMult = args(3) / 100.0
        val scaleMult = args(4) / 100.0
        val maxExtra = Math.min(args(5), atts.size - minCount)
        val effects = (0 to maxExtra).reverse
          .map(extraMatches => {
            val atkMult = baseMult + scaleMult * extraMatches
            val rcvMult = baseRcvMult + scaleMult * extraMatches
            val matches = minCount + extraMatches
            List(ATKBoost(atkMult), RCVBoost(rcvMult))
              .map(payoff =>
                LSComponent(
                  ConditionColorsMatched(
                    atts = atts,
                    matchRequirement = matches
                  ),
                  payoff
                ): LSEffect
              )
              .reduce(_ and _)
          })
          .toList
        firstOf(effects)
      }
      case 166 => {
        val minCount = args(0)
        val baseMult = args(1) / 100.0
        val baseRCVMult = args(2) / 100.0
        val scaleMult = args(3) / 100.0
        val scaleRCVMult = args(4) / 100.0
        val maxExtra = if (scaleMult == 0 && scaleRCVMult == 0) 0 else args(5)
        val effects = (0 to maxExtra).reverse
          .map(extraMatches => {
            val atkmult = baseMult + scaleMult * extraMatches
            val rcvmult = baseRCVMult + scaleRCVMult * extraMatches
            val matches = minCount + extraMatches
            List(ATKBoost(atkmult), RCVBoost(rcvmult))
              .map(payoff =>
                LSComponent(
                  ConditionCombos(matches),
                  payoff
                ): LSEffect
              )
              .reduce(_ and _)
          })
          .toList
        firstOf(effects)
      }
      case 167 => {
        // cbits, min match, atk,rcv, [scale atk,scale rcv, max match]
        val atts = Attribute.fromBitFlag(args(0))
        val minCount = args(1)
        val baseMult = args(2) / 100.0
        val baseRcvMult = args(3) / 100.0
        val scaleMult = args(4) / 100.0
        val rcvscaleMult = args(5) / 100.0
        val maxExtra = Math.max(args(6) - minCount, 0)

        val effects = (0 to maxExtra).reverse
          .map(extraOrbs => {
            val atkMult = baseMult + scaleMult * extraOrbs
            val rcvMult = baseRcvMult + rcvscaleMult * extraOrbs
            val totalOrbs = minCount + extraOrbs
            firstOf(
              atts
                .map(att => ConditionOrbsLinked(totalOrbs, att))
                .map(cond =>
                  LSComponent(cond, ATKBoost(atkMult)) and
                    LSComponent(cond, RCVBoost(rcvMult))
                )
            )
          })
          .toList
        firstOf(effects)
      }
      case 169 => {
        val lambda = LSComponent(ConditionCombos(args(0)), _)
        val atkPayoff =
          if (args(1) != 0) lambda(ATKBoost(args(1) / 100.0))
          else LSEffectNone
        val shieldPayoff =
          if (args(2) != 0) lambda(ShieldRegular(args(2)))
          else LSEffectNone
        atkPayoff and shieldPayoff
      }
      case 170 => {
        val atts = Attribute.fromBitFlag(args(0))
        val minCount = args(1)
        val baseMult = args(2) / 100.0
        val shieldAmount = args(3)
        val scaleMult = args(4) / 100.0
        val maxExtra = Math.min(args(5), atts.size - minCount)
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
        (if (shieldAmount != 0)
           LSComponent(
             ConditionColorsMatched(atts = atts, matchRequirement = minCount),
             ShieldRegular(shieldAmount)
           )
         else
           LSEffectNone) and
          firstOf(effects)
      }
      case 171 => {
        val atts_1 = args.slice(0, 4).map(Attribute.fromBitFlag)
        val atts =
          if (atts_1.exists(_.size > 1)) ???
          else
            atts_1.filter(_.size != 0).map(_.head)
        val minCount = args(4)
        val atkMult = args(5) / 100.0
        val shieldAmount = args(6)
        val atkEffect =
          if (atkMult != 0)
            LSComponent(
              ConditionColorsMatched(atts = atts, matchRequirement = minCount),
              ATKBoost(atkMult)
            )
          else LSEffectNone
        val shieldEffect =
          if (shieldAmount != 0)
            LSComponent(
              ConditionColorsMatched(atts = atts, matchRequirement = minCount),
              ShieldRegular(shieldAmount)
            )
          else LSEffectNone
        atkEffect and shieldEffect
      }
      case 175 => {
        if (args(1) != 0 || args(2) != 0) {
          println("multiple collabs in collab boost?")
          ???
        } else {
          List(HPBoost(_), ATKBoost(_), RCVBoost(_))
            .zip(args.slice(3, 6).map(_ / 100.0))
            .filter(_._2 != 0)
            .map(_(_))
            .map(LSComponent(ConditionCollab(Collab.from(args(0))), _))
            .reduce(_ and _)
        }
      }
      case 177 => {
        val atts = conditionAttributesFromBits(args(0))
        val types = conditionTypesFromBits(args(1))
        val hpMult = args(2) / 100.0
        val atkMult = args(3) / 100.0
        val rcvMult = args(4) / 100.0
        val remainingInit = args(5)
        val baseMult = args(6) / 100.0
        val scaleMult = args(7) / 100.0
        val scalingEffects = (0 to remainingInit)
          .map(remaining => {
            val mult = baseMult + scaleMult * (remainingInit - remaining)
            LSComponent(
              ConditionOrbsRemaining(remaining),
              ATKBoost(mult)
            )
          })
          .toList

        val conditions = atts ++ types
        val passiveEffects = List(HPBoost(_), ATKBoost(_), RCVBoost(_))
          .zip(List(hpMult, atkMult, rcvMult))
          .filter(_._2 != 0)
          .map(_(_))
          .flatMap(payoff => conditions.map(LSComponent(_, payoff): LSEffect))
          .reduce(_ and _)
        LSPassive(NoSkyfall) and passiveEffects and firstOf(scalingEffects)
      }
      case 178 => {
        // seconds,cbits,tbits,hp,atk,rcv,shieldcolours,shield
        val time = args(0)
        val conditions = conditionAttributesFromBits(
          args(1)
        ) ++ conditionTypesFromBits(args(2))
        val payoffs = List(HPBoost(_), ATKBoost(_), RCVBoost(_))
          .zip(args.slice(3, 6).map(_ / 100.0))
          .filter(_._2 != 0)
          .map(_(_))
        val shieldAtts = Attribute.fromBitFlag(args(6))
        val shieldAmt = args(7)

        LSPassive(FixedTime(time)) and conditions
          .cartesian(payoffs)
          .map(LSComponent(_, _): LSEffect)
          .reduceOption(_ and _)
          .getOrElse(LSEffectNone) and
          shieldAtts
            .map(e => LSPassive(ShieldElement(shieldAmt, e)): LSEffect)
            .reduceOption(_ and _)
            .getOrElse(LSEffectNone)
      }
      case 179 => LSEffectNone
      case 182 => {
        val colors = Attribute.fromBitFlag(args(0))
        val minMatch = args(1)
        val atkMult = args(2)
        val shieldAmount = args(3)
        val payoffs = List((i: Int) => ATKBoost(i / 100.0), ShieldRegular(_))
          .zip(List(atkMult, shieldAmount).filter(_ != 0))
          .map(_(_))
        colors
          .map(ConditionAttribute(_))
          .cartesian(payoffs)
          .map((c, p) => LSComponent(c, p): LSEffect)
          .reduce(_ and _)
      }
      case 183 => {
        // cbits, tbits, >=thresh, atk, shield, [<thresh,atk, rcv]
        val conditions = conditionAttributesFromBits(
          args(0)
        ) ++ conditionTypesFromBits(args(1))
        val payoffs1 = List((a: Int) => ATKBoost(a / 100.0), ShieldRegular(_))
          .zip(args.slice(3, 5))
          .filter(_._2 != 0)
          .map(_(_))
        val hpThreshold1 = args(2)

        val payoffs2 = List((a: Int) => ATKBoost(a / 100.0), ShieldRegular(_))
          .zip(args.slice(6, 7))
          .filter(_._2 != 0)
          .map(_(_))
        val hpThreshold2 = args(5)

        val effect1 = conditions
          .cartesian(payoffs1)
          .map((c, p) =>
            LSComponent(
              c and ConditionHPThreshold(
                hpThreshold1,
                isGreater = true,
                allowEqual = true
              ),
              p
            ): LSEffect
          )
          .reduceOption(_ and _)
          .getOrElse(LSEffectNone)

        val effect2 = if (hpThreshold2 != 0) {
          conditions
            .cartesian(payoffs2)
            .map((c, p) =>
              LSComponent(
                c and ConditionHPThreshold(
                  hpThreshold2,
                  isGreater = false,
                  allowEqual = true
                ),
                p
              ): LSEffect
            )
            .reduceOption(_ and _)
            .getOrElse(LSEffectNone)
        } else LSEffectNone
        effect1 and effect2
      }
      case 184 => LSEffectNone

      case 185 => {
        val time = args(0) / 100.0
        val atts = conditionAttributesFromBits(args(1))
        val types = conditionTypesFromBits(args(2))
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
        val conditions = atts ++ types

        payoffs
          .flatMap(payoff =>
            conditions.map(cond => LSComponent(cond, payoff): LSEffect)
          )
          .reduce(_ and _) and (
          if (time > 0) LSPassive(TimeExtend(time)) else LSEffectNone
        )
      }

      case 186 => {
        val conditions = conditionAttributesFromBits(
          args(0)
        ) ++ conditionTypesFromBits(args(1))
        val payoffs = List(HPBoost(_), ATKBoost(_), RCVBoost(_))
          .zip(args.slice(2, 5).map(_ / 100.0))
          .filter(_._2 != 0)
          .map(_(_))
        LSPassive(Board7x6) and conditions
          .cartesian(payoffs)
          .map(LSComponent(_, _))
          .reduce(_ and _)
      }

      case 192 => LSEffectNone
      case 193 => {
        val atts = Attribute.fromBitFlag(args(0))
        val conditions = atts.map(ConditionLMatched(_))
        val payoffs = List(
          (i: Int) => ATKBoost(i / 100.0),
          (i: Int) => RCVBoost(i / 100.0),
          ShieldRegular(_)
        ).zip(args.slice(1, 4))
          .filter(_._2 != 0)
          .map(_(_))
        conditions.cartesian(payoffs).map(LSComponent(_, _)).reduce(_ and _)
      }
      case 194 => {
        val colors = Attribute.fromBitFlag(args(0))
        val matchesRequired = args(1)
        val atkMult = args(2)
        val addedCombos = args(3)
        val condition =
          LSComponent(ConditionColorsMatched(colors, matchesRequired), _)
        Option(atkMult)
          .filter(mult => mult != 0 && mult != 100)
          .map(_ / 100.0)
          .map(ATKBoost(_))
          .map(condition)
          .getOrElse(LSEffectNone) and
          condition(AddCombos(addedCombos))
      }
      case 199 => {
        LSComponent(
          ConditionColorsMatched(
            atts = Attribute.fromBitFlag(args(0)),
            matchRequirement = args(1)
          ),
          BonusAttackFixed(args(2))
        )
      }
      case 200 => {
        val atts = Attribute.fromBitFlag(args(0))
        val linked = args(1)
        val dmg = args(2)
        firstOf(
          atts
            .map(att =>
              LSComponent(
                ConditionOrbsLinked(numOrbs = linked, att = att),
                BonusAttackFixed(dmg)
              )
            )
        )

      }
      case 201 => {
        // colour bit 1, b2, b3, b4, count, damage
        val atts = args
          .slice(0, 4)
          .filter(_ != 0)
          .map(Attribute.fromBitFlag(_))
          .map(_.head)
        LSComponent(
          ConditionColorsMatched(atts, args(4)),
          BonusAttackFixed(args(5))
        )
      }
      case 202 => LSEffectNone
      case 206 => {
        val atts =
          args
            .slice(0, 5)
            .map(Attribute.fromBitFlag(_))
            .filter(_.nonEmpty)
            .map(_.head)
        val minCount = args(5)
        val comboBoost = args(6)
        LSComponent(
          ConditionColorsMatched(atts, minCount),
          AddCombos(comboBoost)
        )
      }
      case 210 => {
        val atts = Attribute.fromBitFlag(args(0))
        val shield = args(1)
        val addedCombos = args(2)
        val crossConditions = atts.map(ConditionCross(_))
        val payoffs: List[LSPayoff] = List(AddCombos(addedCombos)) ++
          (if (shield != 0) List(ShieldRegular(shield)) else List())
        crossConditions
          .cartesian(payoffs)
          .map(LSComponentInfinite(_, _))
          .reduce(_ and _)
      }
      case 223 =>
        LSComponent(ConditionCombos(args(0)), BonusAttackFixed(args(1)))
      case 230 => LSEffectNone
      case 233 => LSEffectNone
      case 234 => LSEffectNone
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

  def conditionAttributesFromBits(bits: Int): List[ConditionAttribute] =
    Attribute.fromBitFlag(bits).map(ConditionAttribute(_))
  def conditionTypesFromBits(bits: Int): List[ConditionType] =
    CardType.fromBitFlag(bits).map(ConditionType(_))

  implicit class ListUtils[A](l1: List[A]) {
    def cartesian[B](
        l2: List[B]
    ): List[(A, B)] = {
      l1.flatMap(i1 => l2.map(i2 => (i1, i2)))
    }
  }

}
