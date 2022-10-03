package skills

import model._
import skills.effects.active._

case class ActiveSkill(
    name: String,
    skillEffect: SkillEffect,
    cd: Int,
    maxLevel: Int
) {
  val maxLevelCd = cd - (maxLevel - 1)
  val cdStr = if (maxLevel == 1) cd else s"$cd-$maxLevelCd"
  override def toString() = {
    skillEffect.toString
  }
}

class SafeList(arr: List[Int]) {
  def apply(i: Int) = {
    try {
      arr(i)
    } catch {
      case e: IndexOutOfBoundsException => 0
    }
  }

  def slice(i: Int, j: Int) =
    (if (arr.size < j) arr ++ List.fill(j)(0) else arr).slice(i, j)
  def indexOf(i: Int) = arr.indexOf(i)
  def map[B](f: Int => B): List[B] = arr.map(f)
  def grouped = arr.grouped

  override def toString = arr.toString
}

object ActiveSkill {
  val NO_SKILL = ActiveSkill("None", NoEffect, 0, 0)

  def fromJson(
      jsonId: Int,
      skillData: Seq[JsonSkillData],
      cardData: Seq[JsonCardData]
  ): ActiveSkill = {
    if (jsonId == 0) NO_SKILL
    else {
      val jsd = skillData(jsonId)
      effectsFromJson(jsd, skillData, cardData)
    }
  }

  def effectsFromJson(
      jsd: JsonSkillData,
      skillData: Seq[JsonSkillData],
      cardData: Seq[JsonCardData]
  ): ActiveSkill = {
    val args = new SafeList(jsd.args)
    val effect: SkillEffect = jsd.internalEffectId match {
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
            to = Attribute.from(args(3))
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
        parseSpike(
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
        ) and SuicideFull
      case 85 =>
        ImmediateDamage(
          DRange(args(1) / 100, args(2) / 100),
          DAttribute(Attribute.from(args(0))),
          DAll
        ) and
          SuicideFull
      case 86 =>
        ImmediateDamage(
          DFixed(args(1)),
          DAttribute(Attribute.from(args(0))),
          DSingle
        ) and
          (if (args(3) == 0) SuicideFull else SuicidePartial(100 - args(3)))
      case 88 =>
        SpikeType(
          multiplier = args(2) / 100.0,
          cardType = CardType.from(args(1)),
          turns = args(0)
        )
      case 90 =>
        args
          .slice(1, 3)
          .map(attInt =>
            parseSpike(
              multiplier = args(3) / 100.0,
              att = Attribute.from(attInt),
              turns = args(0)
            )
          )
          .reduceRight(_ and _)
      case 91 =>
        EnhanceOrbs(Attribute.from(args(0))) and EnhanceOrbs(
          Attribute.from(args(1))
        )
      case 92 => {
        args
          .slice(1, 3)
          .map(CardType.from)
          .map(t =>
            SpikeType(
              multiplier = args(3) / 100.0,
              cardType = t,
              turns = args(0)
            )
          )
          .reduceRight(_ and _)
      }
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
      case 116 => {
        args
          .map(jsdId =>
            effectsFromJson(skillData(jsdId), skillData, cardData).skillEffect
          )
          .reduceRight(_ and _)
      }
      case 117 =>
        val effects = List(
          BindClear(_),
          (x: Int) => HealMultiplier(x / 100),
          HealFlat(_),
          HealPercentMax(_),
          AwokenBindClear(_)
        )
          .zip(args.slice(0, 5))
          .map(t => if (t._2 != 0) t._1(t._2) else NoEffect)
        (effects.tail :+ effects.head)
          .reduceRight(_ and _)
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
          .reduceRight(_ and _)
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
          .reduceRight(_ and _)
      case 132 =>
        if (args(1) != 0) TimeExtendFlat(args(1) / 10, args(0))
        else TimeExtendMult(args(2) / 100.0, args(0))
      case 140 =>
        Attribute.fromBitFlag(args(0)).map(EnhanceOrbs(_)).reduceRight(_ and _)
      case 141 =>
        OrbChangeRandomSpawn(
          numOrbs = args(0),
          spawnAtts = Attribute.fromBitFlag(args(1)),
          exclAtts = Attribute.fromBitFlag(args(2))
        )
      case 142 =>
        AttributeChange(args(0), Attribute.from(args(1)))
      case 143 =>
        ImmediateDamage(
          DTeamHpMult(args(0) / 100),
          DAttribute(Attribute.from(args(2))),
          if (args(1) == 0) DAll else DSingle
        )
      case 144 =>
        ImmediateDamage(
          DTeamAtkMult(args(1) / 100, Attribute.fromBitFlag(args(0))),
          DAttribute(Attribute.from(args(3))),
          if (args(2) == 0) DAll else DSingle
        )
      case 145 => HealByTeamRCV(args(0) / 100)
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
            .reduceRight(_ and _)
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
      case 172 => UnlockOrbs
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
      case 184 => NoSkyfall(args(0))
      case 188 =>
        ImmediateDamage(
          DFixed(args(0)),
          DTrue,
          DSingle
        )
      case 189 => OrbTrace
      case 191 => VoidVoid(args(0))
      case 195 =>
        if (args(0) == 0) SuicideFull else SuicidePartial(100 - args(0))
      case 196 => UnmatchableClear(args(0))
      case 202 => TransformFixed(targetId = args(0), cardData(args(0)).name)
      case 205 =>
        LockedSkyfall(atts = Attribute.fromBitFlag(args(0)), turns = args(1))
      case 207 =>
        if (args.slice(2, 7).exists(_ != 0))
          SpinnerFixed(
            positions = Board.boardPositionsFromBitList(args.slice(2, 7)),
            speed = args(1) / 100.0,
            turns = args(0)
          )
        else
          SpinnerRandom(
            numSpinners = args(7),
            speed = args(1) / 100.0,
            turns = args(0)
          )

      case 208 =>
        OrbChangeRandomSpawn(
          numOrbs = args(0),
          spawnAtts = Attribute.fromBitFlag(args(1)),
          exclAtts = Attribute.fromBitFlag(args(2))
        ) and OrbChangeRandomSpawn(
          numOrbs = args(3),
          spawnAtts = Attribute.fromBitFlag(args(4)),
          exclAtts = (Attribute.fromBitFlag(args(2)) ++
            Attribute.fromBitFlag(args(5))).distinct
        )
      case 214 => UnableToUseSkills(turns = args(0))
      case 215 =>
        SelfUnmatchable(atts = Attribute.fromBitFlag(args(1)), turns = args(0))
      case 218 =>
        if (args(0) == args(1)) AllyDelay(turns = args(0))
        else AllyDelayRange(minTurns = args(0), maxTurns = args(1))
      case 224 =>
        ChangeEnemyAttributeTemporary(att = Attribute.from(1), turns = args(0))
      case 225 =>
        if (args(0) != 0)
          ConditionalComponentHP(hpReq = args(0), needsToBeMore = true)
        else ConditionalComponentHP(hpReq = args(1), needsToBeMore = false)
      case 226 => NailOrbSkyfall(skyfallChance = args(1), turns = args(0))
      case 227 => LeadSwapRightMost
      case 228 => {
        val turns = args(0)
        val atts = Attribute.fromBitFlag(args(1))
        val types = CardType.fromBitFlag(args(2))
        (if (args(3) != 0)
           SpikeScalingByAttributeAndType(
             dmgScaling = args(3) / 100.0,
             atts = atts,
             types = types,
             turns = turns
           )
         else
           NoEffect) and (
          if (args(4) != 0)
            RCVBoostByAttributeAndType(
              rcvScaling = args(4) / 100.0,
              atts = atts,
              types = types,
              turns = turns
            )
          else NoEffect
        )
      }
      case 230 =>
        SpikeSlots(
          multiplier = args(2) / 100.0,
          slots = CardSlot.from(args(1)),
          args(0)
        )
      case 231 =>
        (if (args(6) != 0)
           SpikeScalingByAwakening(
             dmgScaling = args(6) / 100.0,
             awks = args.slice(1, 6).map(Awakening.from),
             turns = args(0)
           )
         else
           NoEffect) and (
          if (args(7) != 0)
            RCVBoostByAwakening(
              rcvScaling = args(7) / 100.0,
              awks = args.slice(1, 6).map(Awakening.from),
              turns = args(0)
            )
          else NoEffect
        )
      case 232 =>
        EvolvingEffect(
          loop = false,
          skills = args.map(jsdId =>
            effectsFromJson(skillData(jsdId), skillData, cardData)
          )
        )
      case 233 =>
        EvolvingEffect(
          loop = true,
          skills = args.map(jsdId =>
            effectsFromJson(skillData(jsdId), skillData, cardData)
          )
        )
      case 234 =>
        if (args(0) != 0)
          ConditionalComponentFloor(floorReq = args(0), needsToBeAfter = true)
        else
          ConditionalComponentFloor(floorReq = args(2), needsToBeAfter = false)
      case 236 =>
        TransformRandom(
          targets = args.map(i => (i, cardData(i).name))
        )
      case 237 => MaxHPMult(args(1) / 100.0, args(0))
      // 8p effects
      case 1000 => {
        println(s"args: $args")
        println(args(2).toBinaryString)
        println("desc: ")
        println(jsd.desc)
        println()
        println()
        println()
        NoEffect
      }
      case n => {
        println(s"Skill id $n not implemented.")
        println(s"args: $args")
        println("desc: ")
        println(jsd.desc)
        ???
      }
    }
    ActiveSkill(
      name = jsd.name,
      skillEffect = effect,
      cd = jsd.initialCooldown.toInt,
      maxLevel = jsd.maxLevel.toInt
    )
  }

  def parseSpike(
      multiplier: Double,
      att: Attribute,
      turns: Int
  ): SkillEffect = {
    if (att == Attribute.HEART)
      RCVBoostMult(multiplier = multiplier, turns = turns)
    else
      SpikeAttribute(
        multiplier = multiplier,
        att = att,
        turns = turns
      )
  }

}
