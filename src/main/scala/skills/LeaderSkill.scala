package skills

import skills.effects.leader._
import model._
import skills.effects.active.ShieldAttribute

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
      case 0 =>
        LSEffectNone
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
        getStatPayoffs(atk = args(1), rcv = args(1))
          .map(payoff =>
            LSComponent(ConditionAttribute(Attribute.from(args(0))), payoff)
          )
          .reduce(_ and _)
      case 29 =>
        getStatPayoffs(hp = args(1), atk = args(1), rcv = args(1))
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
        getStatPayoffs(atk = args(1), rcv = args(2))
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
        getStatPayoffs(
          atk = args(1),
          rcv = args(2)
        ).map(payoff =>
          LSComponent(
            ConditionHPThreshold(
              hpPercent = args(0),
              isGreater = true,
              allowEqual = true
            ),
            payoff
          ): LSEffect
        ).reduceOption(_ and _)
          .getOrElse(LSEffectNone)
      case 45 =>
        getStatPayoffs(
          hp = args(1),
          atk = args(1)
        ).map(payoff =>
          LSComponent(ConditionAttribute(Attribute.from(args(0))), payoff)
        ).reduce(_ and _)
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
      case 53 => LSPassive(DropRateBoost(args(0) / 100.0))
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
        getStatPayoffs(hp = args(1), atk = args(1))
          .map(payoff =>
            LSComponent(ConditionType(CardType.from(args(0))), payoff)
          )
          .reduce(_ and _)
      case 63 =>
        getStatPayoffs(hp = args(1), rcv = args(1))
          .map(payoff =>
            LSComponent(ConditionType(CardType.from(args(0))), payoff)
          )
          .reduce(_ and _)
      case 64 =>
        getStatPayoffs(atk = args(1), rcv = args(1))
          .map(payoff =>
            LSComponent(ConditionType(CardType.from(args(0))), payoff)
          )
          .reduce(_ and _)
      case 65 =>
        getStatPayoffs(hp = args(1), atk = args(1), rcv = args(1))
          .map(payoff =>
            LSComponent(ConditionType(CardType.from(args(0))), payoff)
          )
          .reduce(_ and _)
      case 66 =>
        LSComponent(ConditionCombos(args(0)), ATKBoost(args(1) / 100.0))
      case 67 =>
        getStatPayoffs(hp = args(1), rcv = args(1))
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
        getStatPayoffs(hp = args(2), atk = args(2))
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
          getStatPayoffs(
            atk = args(2),
            rcv = args(2)
          ).map(payoff => LSComponent(cond, payoff))
        ).reduce(_ and _)
      case 76 =>
        List(
          ConditionAttribute(Attribute.from(args(0))),
          ConditionType(CardType.from(args(1)))
        ).flatMap(cond =>
          getStatPayoffs(hp = args(2), atk = args(2), rcv = args(2))
            .map(payoff => LSComponent(cond, payoff))
        ).reduce(_ and _)
      case 77 =>
        args
          .slice(0, 2)
          .map(CardType.from)
          .map(ConditionType(_))
          .flatMap(cond =>
            getStatPayoffs(hp = args(2), atk = args(2))
              .map(payoff => LSComponent(cond, payoff))
          )
          .reduce(_ and _)
      case 79 =>
        args
          .slice(0, 2)
          .map(CardType.from)
          .map(ConditionType(_))
          .flatMap(cond =>
            getStatPayoffs(
              atk = args(2),
              rcv = args(2)
            )
              .map(payoff => LSComponent(cond, payoff))
          )
          .reduce(_ and _)
      case 90 => LSEffectNone // Active skill, 207892
      case 94 =>
        getAtkRcvPayoffsConditional(
          mults = List(args(4), args(4)),
          flags = args.slice(2, 4)
        )
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
        getAtkRcvPayoffsConditional(
          mults = List(args(4), args(4)),
          flags = args.slice(2, 4)
        )
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
        getAtkRcvPayoffsConditional(
          mults = List(args(4), args(4)),
          flags = args.slice(2, 4)
        )
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
        getAtkRcvPayoffsConditional(
          mults = List(args(4), args(4)),
          flags = args.slice(2, 4)
        )
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
        getAtkRcvPayoffsConditional(
          mults = List(args(2), args(2)),
          flags = args.slice(0, 2)
        )
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
        getAtkRcvPayoffsConditional(
          mults = List(args(3), args(3)),
          flags = args.slice(1, 3)
        )
          .map(LSComponent(ConditionCombos(args(0)), _))
          .reduce(_ and _)
      }
      case 104 =>
        getAtkRcvPayoffsConditional(
          mults = List(args(4), args(4)),
          flags = args.slice(2, 4)
        )
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
        getStatPayoffs(atk = args(1), rcv = args(0))
          .map(LSPassive(_))
          .reduce(_ and _)
      case 106 =>
        getStatPayoffs(hp = args(0), atk = args(1))
          .map(LSPassive(_))
          .reduce(_ and _)
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
      case 109 =>
        Attribute
          .fromBitFlag(args(0))
          .map(att =>
            LSComponent(
              ConditionOrbsLinked(
                numOrbs = args(1),
                att = att
              ),
              ATKBoost(args(2) / 100.0)
            )
          )
          .reduce(_ and _)
      case 111 =>
        args
          .slice(0, 2)
          .map(Attribute.from)
          .map(ConditionAttribute(_))
          .flatMap(cond =>
            getStatPayoffs(hp = args(2), atk = args(2))
              .map(payoff => LSComponent(cond, payoff))
          )
          .reduce(_ and _)
      case 114 =>
        args
          .slice(0, 2)
          .map(Attribute.from)
          .map(ConditionAttribute(_))
          .flatMap(cond =>
            getStatPayoffs(hp = args(2), atk = args(2), rcv = args(2))
              .map(payoff => LSComponent(cond, payoff))
          )
          .reduce(_ and _)
      case 115 => LSEffectNone
      case 116 => LSEffectNone
      case 119 | 159 => {
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
        val payoffs = getStatPayoffs(hp = args(2), atk = args(3), rcv = args(4))
        conditions
          .cartesian(payoffs)
          .map(LSComponent(_, _): LSEffect)
          .reduceOption(_ and _)
          .getOrElse(LSEffectNone)
      case 122 => {
        val conditions = conditionAttributesFromBits(args(1)) ++
          conditionTypesFromBits(args(2))
        val payoffs = getStatPayoffs(atk = args(3), rcv = args(4))
        conditions
          .cartesian(payoffs)
          .map((c, p) =>
            LSComponent(
              c and ConditionHPThreshold(
                args(0),
                isGreater = false,
                allowEqual = true
              ),
              p
            ): LSEffect
          )
          .reduceOption(_ and _)
          .getOrElse(LSEffectNone)
      }
      case 123 => {
        val conditions =
          conditionAttributesFromBits(args(1)) ++
            conditionTypesFromBits(args(2))
        conditions
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
      }
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
        getStatPayoffs(hp = args(5), atk = args(6), rcv = args(7))
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
          val resistAtts = Attribute.fromBitFlag(args(5))
          val resistance = args(6)
          val payoffs =
            getStatPayoffs(hp = args(2), atk = args(3), rcv = args(4))
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
        val resistAtts = Attribute.fromBitFlag(args(5))
        val resistance = args(6)
        val payoffs =
          getStatPayoffs(atk = args(3), rcv = args(4))
        val conditions = atts ++ types
        conditions
          .cartesian(
            (payoffs ++ resistAtts.map(att => ShieldElement(resistance, att)))
          )
          .map((cond, payoff) =>
            LSComponent(
              ConditionHPThreshold(
                threshold,
                isGreater = false,
                allowEqual = true
              ) and cond,
              payoff
            ): LSEffect
          )
          .reduceOption(_ and _)
          .getOrElse(LSEffectNone)
      }
      case 131 => {
        val threshold = args(0)
        val conditions = conditionAttributesFromBits(args(1))
          ++ conditionTypesFromBits(args(2))
        val payoffs = getStatPayoffs(atk = args(3), rcv = args(4))
          ++ Attribute
            .fromBitFlag(args(5))
            .map(att => ShieldElement(args(6), att))

        conditions
          .cartesian(payoffs)
          .map((cond, payoff) =>
            LSComponent(
              ConditionHPThreshold(
                threshold,
                isGreater = true,
                allowEqual = true
              ) and cond,
              payoff
            ): LSEffect
          )
          .reduceOption(_ and _)
          .getOrElse(LSEffectNone)
      }
      case 132 => LSEffectNone
      case 133 => {
        val atts = conditionAttributesFromBits(args(0))
        val types = conditionTypesFromBits(args(1))
        val payoffs = getStatPayoffs(atk = args(2), rcv = args(3))
        val conditions = atts ++ types

        conditions
          .cartesian(payoffs)
          .map((cond, payoff) =>
            LSComponent(
              ConditionSkillUsed and cond,
              payoff
            ): LSEffect
          )
          .reduce(_ and _)
      }
      case 136 => {
        val boost1 = args.slice(0, 4)
        val boost2 = args.slice(4, 8)
        List(boost1, boost2)
          .flatMap(boost =>
            conditionAttributesFromBits(boost(0))
              .cartesian(
                getStatPayoffs(
                  hp = boost(1),
                  atk = boost(2),
                  rcv = boost(3)
                )
              )
              .map(LSComponent(_, _))
          )
          .reduce(_ and _)
      }
      case 137 => {
        val boost1 = args.slice(0, 4)
        val boost2 = args.slice(4, 8)
        List(boost1, boost2)
          .flatMap(boost =>
            getStatPayoffs(
              hp = boost(1),
              atk = boost(2),
              rcv = boost(3)
            ).map(
              LSComponent(
                ConditionType(CardType.firstFromBitFlag(boost(0))),
                _
              )
            )
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
      case 149 =>
        LSComponent(
          ConditionOrbsLinkedExact(numOrbs = 4, att = Attribute.HEART),
          RCVBoost(args(0) / 100.0)
        )
      case 150 => LSComponent(ConditionSparkle, ATKBoost(args(1) / 100.0))
      case 151 =>
        getStatPayoffs(atk = args(0), rcv = args(1), shield = args(2))
          .map(LSComponent(ConditionCross(Attribute.HEART), _))
          .reduce(_ and _)
      case 155 => {
        val conds = conditionAttributesFromBits(
          args(0)
        ) ++ conditionTypesFromBits(args(1))
        val payoffs = getStatPayoffs(hp = args(2), atk = args(3), rcv = args(4))
        conds
          .cartesian(payoffs)
          .map((c, p) =>
            LSComponent(
              c and ConditionCoop,
              p
            )
          )
          .reduce(_ and _)
      }
      case 157 =>
        LSComponentInfinite(
          ConditionCross(Attribute.from(args(0))),
          ATKBoost(args(1) / 100.0)
        )
      case 158 => {
        val minMatch = args(0)
        val conditions = conditionAttributesFromBits(args(1))
          ++ conditionTypesFromBits(args(2))
        val payoffs = getStatPayoffs(
          hp = args(4),
          atk = args(3),
          rcv = args(5)
        ) // apparently hp and atk order switched here????
        LSPassive(MinimumMatch(minMatch)) and conditions
          .cartesian(payoffs)
          .map(LSComponent(_, _))
          .reduce(_ and _)
      }
      // case 159 => see case 119
      case 162 => LSPassive(Board7x6)
      case 163 => {
        val conditions = conditionAttributesFromBits(args(0))
          ++ conditionTypesFromBits(args(1))
        val payoffs = getStatPayoffs(hp = args(2), atk = args(3), rcv = args(4))
          ++ (if (args(5) != 0) {
                Attribute
                  .fromBitFlag(args(5))
                  .map(att => ShieldElement(args(6), att))
              } else List())
        LSPassive(NoSkyfall) and conditions
          .cartesian(payoffs)
          .map(LSComponent(_, _): LSEffect)
          .reduceOption(_ and _)
          .getOrElse(LSEffectNone)
      }
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
          getStatPayoffs(hp = args(3), atk = args(4), rcv = args(5))
            .map(LSComponent(ConditionCollab(Collab.from(args(0))), _))
            .reduce(_ and _)
        }
      }
      case 177 => {
        val atts = conditionAttributesFromBits(args(0))
        val types = conditionTypesFromBits(args(1))
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
        val passiveEffects =
          getStatPayoffs(hp = args(2), atk = args(3), rcv = args(4))
            .flatMap(payoff => conditions.map(LSComponent(_, payoff): LSEffect))
            .reduceOption(_ and _)
            .getOrElse(LSEffectNone)
        LSPassive(NoSkyfall) and passiveEffects and firstOf(scalingEffects)
      }
      case 178 => {
        // seconds,cbits,tbits,hp,atk,rcv,shieldcolours,shield
        val time = args(0)
        val conditions = conditionAttributesFromBits(
          args(1)
        ) ++ conditionTypesFromBits(args(2))
        val payoffs = getStatPayoffs(hp = args(3), atk = args(4), rcv = args(5))
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
        val payoffs = getStatPayoffs(hp = args(3), atk = args(4), rcv = args(5))
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
        val payoffs = getStatPayoffs(hp = args(2), atk = args(3), rcv = args(4))
        LSPassive(Board7x6) and conditions
          .cartesian(payoffs)
          .map(LSComponent(_, _): LSEffect)
          .reduceOption(_ and _)
          .getOrElse(LSEffectNone)
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
      case 197 => LSPassive(VoidPoisonDamage)
      case 198 => {
        val healAmt = args(0)
        val payoffs = getStatPayoffs(atk = args(1), shield = args(2))
          ++ (if (args(3) != 0) List(PayoffAwokenBindClear(args(3)))
              else List())
        payoffs
          .map(payoff => LSComponent(Healed(healAmt), payoff))
          .reduce(_ and _)
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
      case 203 => {
        getStatPayoffs(hp = args(1), atk = args(2), rcv = args(3))
          .map(payoff =>
            LSComponent(
              args(0) match {
                case 0 => ConditionAllPixel
                case 2 => ConditionAllRevo
              },
              payoff
            )
          )
          .reduce(_ and _)
      }
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
      case 209 =>
        LSComponent(ConditionCross(Attribute.HEART), AddCombos(args(0)))
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
      case 213 =>
        (conditionAttributesFromBits(args(0)) ++ conditionTypesFromBits(
          args(1)
        ))
          .map(cond => LSComponent(cond, AddAwakening(Awakening.from(args(2)))))
          .reduce(_ and _)

      case 217 =>
        getStatPayoffs(hp = args(1), atk = args(2), rcv = args(3))
          .map(payoff => LSComponent(ConditionTeamRarity(args(0)), payoff))
          .reduce(_ and _)
      case 219 =>
        Attribute
          .fromBitFlag(args(0))
          .map(att =>
            LSComponent(
              ConditionOrbsLinked(numOrbs = args(1), att = att),
              AddCombos(args(2))
            )
          )
          .reduce(_ and _)
      case 220 =>
        Attribute
          .fromBitFlag(args(0))
          .map(att =>
            LSComponent(ConditionLMatched(att = att), AddCombos(args(1)))
          )
          .reduce(_ and _)
      case 221 =>
        Attribute
          .fromBitFlag(args(0))
          .map(att =>
            LSComponent(ConditionLMatched(att = att), BonusAttackFixed(args(1)))
          )
          .reduce(_ and _)

      case 223 =>
        LSComponent(ConditionCombos(args(0)), BonusAttackFixed(args(1)))
      case 229 =>
        LSAttOrTypeScaling(
          atts = Attribute.fromBitFlag(args(0)),
          types = CardType.fromBitFlag(args(1)),
          hpScaling = args(2) / 100.0,
          atkScaling = args(3) / 100.0,
          rcvScaling = args(4) / 100.0
        )
      case 230 => LSEffectNone
      case 233 => LSEffectNone
      case 234 => LSEffectNone
      case 235 => {
        val atts = Attribute.fromBitFlag(args(0))
        val amt = args(2)
        val atk = args(3)
        val addCombo = args(5)
        val payoffs = getStatPayoffs(atk = atk) :+ AddCombos(addCombo)
        atts
          .cartesian(payoffs)
          .map((att, payoff) =>
            LSComponentInfinite(
              ConditionOrbsLinkedExact(numOrbs = amt, att = att),
              payoff
            )
          )
          .reduce(_ and _)
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

  def conditionAttributesFromBits(bits: Int): List[ConditionAttribute] =
    Attribute.fromBitFlag(bits).map(ConditionAttribute(_))
  def conditionTypesFromBits(bits: Int): List[ConditionType] =
    CardType.fromBitFlag(bits).map(ConditionType(_))

  def getAtkRcvPayoffsConditional(
      mults: List[Int],
      flags: List[Int]
  ): List[LSPayoff] = {
    if (mults.size == flags.size && flags.size == 2) {
      val atkMult = if (flags(0) != 0) mults(0) else 0
      val rcvMult = if (flags(1) != 0) mults(1) else 0
      getStatPayoffs(atk = atkMult, rcv = rcvMult)
    } else {
      ???
    }
  }

  def getStatPayoffs(
      hp: Int = 0,
      atk: Int = 0,
      rcv: Int = 0,
      shield: Int = 0
  ): List[LSPayoff] = {
    val payoffLambdas: List[Int => LSPayoff] = List(
      (i: Int) => HPBoost(i / 100.0),
      (i: Int) => ATKBoost(i / 100.0),
      (i: Int) => RCVBoost(i / 100.0),
      ShieldRegular(_)
    )
    val invalidValues = List(List(0, 100), List(0, 100), List(0, 100), List(0))
    val inputs = List(hp, atk, rcv, shield)
    (inputs, invalidValues, payoffLambdas).zipped.toList
      .filter(t => !t._2.contains(t._1))
      .map(t => t._3(t._1))
  }
  implicit class ListUtils[A](l1: List[A]) {
    def cartesian[B](
        l2: List[B]
    ): List[(A, B)] = {
      l1.flatMap(i1 => l2.map(i2 => (i1, i2)))
    }
  }

}
