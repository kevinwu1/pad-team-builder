package skills

import model.JsonSkillData
import model.Card
import model.JsonCardData
import skills.effects._
import model.Attribute
import model.CardType
import model.Awakening

case class ActiveSkill(
    name: String,
    skillEffect: SkillEffect,
    cd: Int,
    maxLevel: Int
) {
  val maxLevelCd = cd - (maxLevel - 1)
  override def toString() = {
    skillEffect.toString
  }
}

object ActiveSkill {
  def fromJson(
      jsonId: Int,
      skillData: Array[JsonSkillData],
      cardData: Array[JsonCardData]
  ): ActiveSkill = {
    val jsd = skillData(jsonId)
    val skillEffect = effectsFromJson(jsd, skillData, cardData)
    ActiveSkill(
      name = jsd.name,
      skillEffect = skillEffect,
      cd = jsd.initialCooldown.toInt,
      maxLevel = jsd.maxLevel.toInt
    )
  }

  def effectsFromJson(
      jsd: JsonSkillData,
      skillData: Array[JsonSkillData],
      cardData: Array[JsonCardData]
  ): SkillEffect = {
    val args = jsd.args
    jsd.internalEffectId match {
      case 0 =>
        ImmediateDamage(
          DMultiplier(args(1) / 100),
          DAttribute(Attribute.from(args(0))),
          DAll
        )
      case 1 =>
        ImmediateDamage(
          DFixed(args(1)),
          DAttribute(Attribute.fromOrdinal(args(0))),
          DAll
        )
      case 2 =>
        ImmediateDamage(
          DMultiplier(args(0) / 100),
          DInherit,
          DSingle
        )
      case 3 => ShieldAll(percent = args(1), turns = args(0))
      case 4 => Poison(args(0) / 100)
      case 5 => ChangeTheWorld(args(0))
      case 6 => GravityFalse(args(0))
      case 7 => HealMultiplier(args(0) / 100)
      case 8 => HealFlat(args(0))
      case 9 =>
        OrbChangeAtoB(from = Attribute.from(args(0)), Attribute.from(args(1)))
      case 10 => Refresh
      case 18 => Delay(args(0))
      case 19 => DefenseBreak(percent = args(1), turns = args(0))
      case 20 =>
        OrbChangeAtoB(
          from = Attribute.from(args(0)),
          to = Attribute.from(args(1))
        ) and
          OrbChangeAtoB(
            from = Attribute.from(args(2)),
            to = Attribute.from(args(2))
          )
      case 21 =>
        ShieldAttribute(
          turns = args(0),
          attribute = Attribute.from(args(1)),
          percent = args(2)
        )
      case 35 =>
        ImmediateDamage(
          DMultiplier(args(0) / 100),
          DInherit,
          DSingle,
          Some(args(1))
        )
      case 37 =>
        ImmediateDamage(
          DMultiplier(args(1) / 100),
          DAttribute(Attribute.from(args(0))),
          DSingle
        )
      case 42 =>
        ImmediateDamage(
          DFixed(args(2)),
          DAttribute(Attribute.from(args(1))),
          DAttributeTarget(Attribute.from(args(0)))
        )
      case 50 =>
        SpikeAttribute(
          multiplier = args(2) / 100.0,
          att = Attribute.from(args(1)),
          turns = args(0)
        )
      case 51 => MassAttack(args(0))
      case 52 => EnhanceOrbs(Attribute.from(args(0)))
      case 55 =>
        ImmediateDamage(
          DFixed(args(0)),
          DTrue,
          DSingle
        )
      case 56 =>
        ImmediateDamage(
          DFixed(args(0)),
          DTrue,
          DAll
        )
      case 58 =>
        ImmediateDamage(
          DRange(args(1) / 100, args(2) / 100),
          DAttribute(Attribute.from(args(0))),
          DAll
        )
      case 59 =>
        ImmediateDamage(
          DRange(args(1) / 100, args(2) / 100),
          DAttribute(Attribute.from(args(0))),
          DSingle
        )
      case 60 =>
        CounterAttack(
          multiplier = args(1) / 100,
          att = Attribute.from(args(2)),
          turns = args(0)
        )
      case 71 =>
        OrbChangeFullBoard(args.slice(0, args.indexOf(-1)).map(Attribute.from))
      case 84 =>
        ImmediateDamage(
          DRange(args(1) / 100, args(2) / 100),
          DAttribute(Attribute.from(args(0))),
          DSingle
        ) and FullSuicide
      case 85 =>
        ImmediateDamage(
          DRange(args(1) / 100, args(2) / 100),
          DAttribute(Attribute.from(args(0))),
          DAll
        ) and
          FullSuicide
      case 88 =>
        SpikeType(
          multiplier = args(2) / 100.0,
          cardType = CardType.from(args(1)),
          turns = args(0)
        )
      case 90 =>
        SpikeAttribute(
          multiplier = args(3) / 100.0,
          att = Attribute.from(args(1)),
          turns = args(0)
        ) and SpikeAttribute(
          multiplier = args(3) / 100.0,
          att = Attribute.from(args(2)),
          turns = args(0)
        )
      case 91 =>
        EnhanceOrbs(Attribute.from(args(0))) and EnhanceOrbs(
          Attribute.from(args(1))
        )
      case 93 => LeadSwap
      case 110 =>
        ImmediateDamage(
          DGrudge(hpMaxMult = args(2) / 100, hp1Mult = args(3) / 100),
          DAttribute(Attribute.from(args(1))),
          if (args(0) == 0) DAll else DSingle
        )
      case 115 =>
        ImmediateDamage(
          DMultiplier(args(1) / 100),
          DAttribute(Attribute.from(args(0))),
          DSingle,
          Some(args(2))
        )
      case 116 =>
        args
          .map(jsdId => effectsFromJson(skillData(jsdId), skillData, cardData))
          .reduce(_ and _)
      case 117 =>
        val effects = List(
          BindClear(_),
          HealMultiplier(_),
          HealFlat(_),
          HealPercentMax(_),
          AwokenBindClear(_)
        )
          .zip(args.slice(0, 5))
          .map(t => if (t._2 != 0) t._1(t._2) else NoEffect)
        (effects.tail :+ effects.head)
          .reduce(_ and _)
      case 118 => Random(args.map(fromJson(_, skillData, cardData)))
      case 126 =>
        IncreaseSkyfall(
          colors = Attribute.fromBitFlag(args(0)),
          percent = args(3),
          turns = args(2)
        )
      case 127 =>
        args
          .grouped(2)
          .flatMap(l =>
            Column
              .fromBitFlag(l(0).toInt)
              .map(col =>
                OrbChangeColumn(col, Attribute.firstFromBitFlag(l(1).toInt))
              )
          )
          .reduce(_ and _)
      case 128 =>
        args
          .grouped(2)
          .flatMap(l =>
            Row
              .fromBitFlag(l(0).toInt)
              .map(row =>
                OrbChangeRow(row, Attribute.firstFromBitFlag(l(1).toInt))
              )
          )
          .reduce(_ and _)
      case 132 =>
        if (args(1) != 0) TimeExtendFlat(args(1) / 10, args(0))
        else TimeExtendMult(args(2) / 100.0, args(0))
      case 141 =>
        OrbChangeRandomSpawn(
          args(0),
          Attribute.fromBitFlag(args(1)),
          Attribute.fromBitFlag(args(2))
        )
      case 142 =>
        AttributeChange(args(0), Attribute.from(args(1)))
      case 144 =>
        ImmediateDamage(
          DTeamAtkMult(args(1) / 100, Attribute.fromBitFlag(args(0))),
          DAttribute(Attribute.from(args(3))),
          if (args(2) == 0) DAll else DSingle
        )
      case 146 =>
        if (args(0) == args(1)) HasteFixed(args(0))
        else HasteRandom(args(0), args(1))
      case 152 =>
        LockOrbs(Attribute.fromBitFlag(args(0)), args(1))
      case 153 => ChangeEnemyAttributePermanent(Attribute.from(args(0)))
      case 154 => {
        val inputColors = Attribute.fromBitFlag(args(0))
        val outputColors = Attribute.fromBitFlag(args(1))
        if (outputColors.size > 1)
          OrbChangeMultiTarget(inputColors, outputColors)
        else
          inputColors
            .map(c => OrbChangeAtoB(c, outputColors.head))
            .reduce(_ and _)
      }
      case 156 =>
        args(4) match {
          case 1 =>
            HealScalingByAwakening(
              args(5) / 100,
              args.slice(1, 4).map(Awakening.from).filter(_ != Awakening.None)
            )
          case 2 =>
            SpikeScalingByAwakening(
              (args(5) - 100) / 100.0,
              args.slice(1, 4).map(Awakening.from).filter(_ != Awakening.None),
              args(0)
            )
          case 3 =>
            ShieldScalingByAwakening(
              (args(5) - 100) / 100.0,
              args.slice(1, 4).map(Awakening.from).filter(_ != Awakening.None),
              args(0)
            )
        }
      case 160 => AddCombos(args(1), args(0))
      case 161 => GravityTrue(args(0))
      case 168 =>
        SpikeScalingByAwakening(
          dmgScaling = args(7),
          awks = List(Awakening.from(args(1))),
          turns = args(0)
        )
      case 173 =>
        (if (args(1) != 0) VoidAttributeAbsorb(args(0)) else NoEffect) and
          (if (args(3) != 0) VoidDamageAbsorb(args(0)) else NoEffect)
      case 176 =>
        OrbChangePattern(
          args
            .slice(0, 5)
            .map(bits => {
              (0 to 5)
                .map(i => ((1 << i) & bits) != 0)
                .toList
            }),
          Attribute.from(args(5))
        )
      case 179 => HealPerTurn(args(2), args(0))
      case 180 => EnhancedSkyfall(args(1), args(0))
      case 189 => OrbTrace
      case 202 => Transform(targetId = args(0), cardData(args(0)).name)
      case 218 =>
        if (args(0) == args(1)) AllyDelay(args(0))
        else AllyDelayRange(args(0), args(1))
      case 224 => ChangeEnemyAttributeTemporary(Attribute.from(1), args(0))
      case 227 => LeadSwapRightMost
      case n => {
        println(s"Skill id $n not implemented.")
        println(s"args: $args")
        println("desc: ")
        println(jsd.desc)
        ???
      }
    }
  }

}
