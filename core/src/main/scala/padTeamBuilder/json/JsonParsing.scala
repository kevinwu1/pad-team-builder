package padTeamBuilder.json

import padTeamBuilder.model._
import padTeamBuilder.skills._
import play.api.libs.json._
import play.api.libs.functional.syntax._
import padTeamBuilder.skills.effects.active._
import padTeamBuilder.skills.effects.leader._

object JsonParsing {

  def makeEnumWrites[T <: Enum[T]]: Writes[T] = new Writes[T] {
    def writes(e: T) = JsNumber(e.ordinal)
  }
  def makeEnumReads[T <: Enum[T]](en: Int => T): Reads[T] = new Reads[T] {
    override def reads(json: JsValue): JsResult[T] = JsSuccess(
      en(
        json.as[JsNumber].value.toInt
      )
    )
  }

  implicit val attWrites: Writes[Attribute] = makeEnumWrites[Attribute]
  implicit val cardTypeWrites: Writes[CardType] = makeEnumWrites[CardType]
  implicit val awakeningWrites: Writes[Awakening] = makeEnumWrites[Awakening]
  implicit val cardSlotWrites: Writes[CardSlot] = makeEnumWrites[CardSlot]
  implicit val columnWrites: Writes[Column] = makeEnumWrites[Column]
  implicit val rowWrites: Writes[Row] = makeEnumWrites[Row]

  implicit val attReads: Reads[Attribute] =
    makeEnumReads[Attribute](Attribute.fromOrdinal)
  implicit val cardTypeReads: Reads[CardType] =
    makeEnumReads[CardType](CardType.fromOrdinal)
  implicit val awakeningReads: Reads[Awakening] =
    makeEnumReads[Awakening](Awakening.fromOrdinal)
  implicit val cardSlotReads: Reads[CardSlot] =
    makeEnumReads[CardSlot](CardSlot.fromOrdinal)
  implicit val columnReads: Reads[Column] =
    makeEnumReads[Column](Column.fromOrdinal)
  implicit val rowReads: Reads[Row] = makeEnumReads[Row](Row.fromOrdinal)

  // implicit val attFormat: Format[Attribute] = Json.format[Attribute]
  // implicit val cardTypeFormat: Format[CardType] = Json.format[CardType]
  // implicit val awakeningFormat: Format[Awakening] = Json.format[Awakening]

  implicit val asFormat: Format[ActiveSkill] = Json.format[ActiveSkill]
  // implicit val activeSkillWrites: Writes[ActiveSkill] = (
  //   (JsPath \ "name").write[String] and
  //     (JsPath \ "skillEffect").write[SkillEffect] and
  //     (JsPath \ "cd").write[Int] and
  //     (JsPath \ "maxLevel").write[Int]
  // )((as: ActiveSkill) =>
  //   (
  //     as.name,
  //     as.skillEffect,
  //     as.cd,
  //     as.maxLevel
  //   )
  // )

  implicit val ccFormat: Format[ConditionalComponent] =
    Json.format[ConditionalComponent]

  implicit val formatNoEffect: Format[NoEffect] = Json.format[NoEffect]
  implicit val formatMultiEffect: Format[MultiEffect] = Json.format[MultiEffect]
  implicit val formatEvolvingEffect: Format[EvolvingEffect] =
    Json.format[EvolvingEffect]
  implicit val formatConditionalEffect: Format[ConditionalEffect] =
    Json.format[ConditionalEffect]
  implicit val formatChangeTheWorld: Format[ChangeTheWorld] =
    Json.format[ChangeTheWorld]
  implicit val formatCounterAttack: Format[CounterAttackSkill] =
    Json.format[CounterAttackSkill]
  implicit val formatSuicidePartial: Format[SuicidePartial] =
    Json.format[SuicidePartial]
  implicit val formatSuicideFull: Format[SuicideFull] = Json.format[SuicideFull]
  implicit val formatDefenseBreak: Format[DefenseBreak] =
    Json.format[DefenseBreak]
  implicit val formatDelay: Format[Delay] = Json.format[Delay]
  implicit val formatEnhanceOrbs: Format[EnhanceOrbs] = Json.format[EnhanceOrbs]
  implicit val formatGravityFalse: Format[GravityFalse] =
    Json.format[GravityFalse]
  implicit val formatGravityTrue: Format[GravityTrue] = Json.format[GravityTrue]
  implicit val formatHealFlat: Format[HealFlat] = Json.format[HealFlat]
  implicit val formatHealMultiplier: Format[HealMultiplier] =
    Json.format[HealMultiplier]
  implicit val formatHealPercentMax: Format[HealPercentMax] =
    Json.format[HealPercentMax]
  implicit val formatHealScalingByAwakening: Format[HealScalingByAwakening] =
    Json.format[HealScalingByAwakening]
  implicit val formatHealByTeamRCV: Format[HealByTeamRCV] =
    Json.format[HealByTeamRCV]
  implicit val formatHealPerTurn: Format[HealPerTurn] = Json.format[HealPerTurn]
  implicit val formatIncreaseSkyfall: Format[IncreaseSkyfall] =
    Json.format[IncreaseSkyfall]
  implicit val formatMassAttack: Format[MassAttack] = Json.format[MassAttack]
  implicit val formatOrbChangeAtoB: Format[OrbChangeAtoB] =
    Json.format[OrbChangeAtoB]
  implicit val formatOrbChangeFullBoard: Format[OrbChangeFullBoard] =
    Json.format[OrbChangeFullBoard]
  implicit val formatOrbChangeColumn: Format[OrbChangeColumn] =
    Json.format[OrbChangeColumn]
  implicit val formatOrbChangeColumnRandom: Format[OrbChangeColumnRandom] =
    Json.format[OrbChangeColumnRandom]
  implicit val formatOrbChangeRow: Format[OrbChangeRow] =
    Json.format[OrbChangeRow]
  implicit val formatOrbChangeRandomSpawn: Format[OrbChangeRandomSpawn] =
    Json.format[OrbChangeRandomSpawn]
  implicit val formatPoison: Format[Poison] = Json.format[Poison]
  implicit val formatRefresh: Format[Refresh] = Json.format[Refresh]
  implicit val formatRCVBoostMult: Format[RCVBoostMult] =
    Json.format[RCVBoostMult]
  implicit val formatRCVBoostByAwakening: Format[RCVBoostByAwakening] =
    Json.format[RCVBoostByAwakening]
  implicit val formatRCVBoostByAttributeAndType
      : Format[RCVBoostByAttributeAndType] =
    Json.format[RCVBoostByAttributeAndType]
  implicit val formatShieldAll: Format[ShieldAll] = Json.format[ShieldAll]
  implicit val formatShieldAttribute: Format[ShieldAttribute] =
    Json.format[ShieldAttribute]
  implicit val formatShieldScalingByAwakening
      : Format[ShieldScalingByAwakening] = Json.format[ShieldScalingByAwakening]
  implicit val formatSpikeAttribute: Format[SpikeAttribute] =
    Json.format[SpikeAttribute]
  implicit val formatSpikeType: Format[SpikeType] = Json.format[SpikeType]
  implicit val formatSpikeScalingByAwakening: Format[SpikeScalingByAwakening] =
    Json.format[SpikeScalingByAwakening]
  implicit val formatSpikeScalingByAttributeAndType
      : Format[SpikeScalingByAttributeAndType] =
    Json.format[SpikeScalingByAttributeAndType]
  implicit val formatSpikeSlots: Format[SpikeSlots] = Json.format[SpikeSlots]
  implicit val formatTransformFixed: Format[TransformFixed] =
    Json.format[TransformFixed]
  implicit val formatTransformRandom: Format[TransformRandom] =
    Json.format[TransformRandom]
  implicit val formatLeadSwap: Format[LeadSwapThisCard] =
    Json.format[LeadSwapThisCard]
  implicit val formatLeadSwapRightMost: Format[LeadSwapRightMost] =
    Json.format[LeadSwapRightMost]
  implicit val formatAwokenBindClear: Format[AwokenBindClear] =
    Json.format[AwokenBindClear]
  implicit val formatBindClear: Format[BindClear] = Json.format[BindClear]
  implicit val formatRandom: Format[Random] = Json.format[Random]
  implicit val formatTimeExtendFlat: Format[TimeExtendFlat] =
    Json.format[TimeExtendFlat]
  implicit val formatTimeExtendMult: Format[TimeExtendMult] =
    Json.format[TimeExtendMult]
  implicit val formatAttributeChange: Format[AttributeChange] =
    Json.format[AttributeChange]
  implicit val formatHasteFixed: Format[HasteFixed] = Json.format[HasteFixed]
  implicit val formatHasteRandom: Format[HasteRandom] = Json.format[HasteRandom]
  implicit val formatLockOrbs: Format[LockOrbs] = Json.format[LockOrbs]
  implicit val formatChangeEnemyAttributePermanent
      : Format[ChangeEnemyAttributePermanent] =
    Json.format[ChangeEnemyAttributePermanent]
  implicit val formatChangeEnemyAttributeTemporary
      : Format[ChangeEnemyAttributeTemporary] =
    Json.format[ChangeEnemyAttributeTemporary]
  implicit val formatOrbChangeMultiTarget: Format[OrbChangeMultiTarget] =
    Json.format[OrbChangeMultiTarget]
  implicit val formatAddCombos: Format[AddCombosSkill] =
    Json.format[AddCombosSkill]
  implicit val formatVoidDamageAbsorb: Format[VoidDamageAbsorb] =
    Json.format[VoidDamageAbsorb]
  implicit val formatVoidAttributeAbsorb: Format[VoidAttributeAbsorb] =
    Json.format[VoidAttributeAbsorb]
  implicit val formatVoidVoid: Format[VoidVoid] = Json.format[VoidVoid]
  implicit val formatOrbChangePattern: Format[OrbChangePattern] =
    Json.format[OrbChangePattern]
  implicit val formatEnhancedSkyfall: Format[EnhancedSkyfall] =
    Json.format[EnhancedSkyfall]
  implicit val formatOrbTrace: Format[OrbTrace] = Json.format[OrbTrace]
  implicit val formatAllyDelay: Format[AllyDelay] = Json.format[AllyDelay]
  implicit val formatAllyDelayRange: Format[AllyDelayRange] =
    Json.format[AllyDelayRange]
  implicit val formatUnlockOrbs: Format[UnlockOrbs] = Json.format[UnlockOrbs]
  implicit val formatUnmatchableClear: Format[UnmatchableClear] =
    Json.format[UnmatchableClear]
  implicit val formatNoSkyfall: Format[NoSkyfall] = Json.format[NoSkyfall]
  implicit val formatConditionalComponentHP: Format[ConditionalComponentHP] =
    Json.format[ConditionalComponentHP]
  implicit val formatConditionalComponentFloor
      : Format[ConditionalComponentFloor] =
    Json.format[ConditionalComponentFloor]
  implicit val formatSpinnerRandom: Format[SpinnerRandom] =
    Json.format[SpinnerRandom]
  implicit val formatSpinnerFixed: Format[SpinnerFixed] =
    Json.format[SpinnerFixed]
  implicit val formatLockedSkyfall: Format[LockedSkyfall] =
    Json.format[LockedSkyfall]
  implicit val formatUnableToUseSkills: Format[UnableToUseSkills] =
    Json.format[UnableToUseSkills]
  implicit val formatSelfUnmatchable: Format[SelfUnmatchable] =
    Json.format[SelfUnmatchable]
  implicit val formatNailOrbSkyfall: Format[NailOrbSkyfall] =
    Json.format[NailOrbSkyfall]
  implicit val formatMaxHPMult: Format[MaxHPMult] = Json.format[MaxHPMult]
  implicit val formatImmediateDamage: Format[ImmediateDamage] =
    Json.format[ImmediateDamage]
  implicit val formatDFixed: Format[DFixed] = Json.format[DFixed]
  implicit val formatDMultiplier: Format[DMultiplier] = Json.format[DMultiplier]
  implicit val formatDRange: Format[DRange] = Json.format[DRange]
  implicit val formatDGrudge: Format[DGrudge] = Json.format[DGrudge]
  implicit val formatDTeamAtkMult: Format[DTeamAtkMult] =
    Json.format[DTeamAtkMult]
  implicit val formatDTeamHpMult: Format[DTeamHpMult] = Json.format[DTeamHpMult]
  implicit val formatDAttribute: Format[DAttribute] = Json.format[DAttribute]
  implicit val formatDTrue: Format[DTrue] = Json.format[DTrue]
  implicit val formatDInherit: Format[DInherit] = Json.format[DInherit]
  implicit val formatDSingle: Format[DSingle] = Json.format[DSingle]
  implicit val formatDAll: Format[DAll] = Json.format[DAll]
  implicit val formatDAttributeTarget: Format[DAttributeTarget] =
    Json.format[DAttributeTarget]
  implicit val formatTimeReducedForTop2: Format[TimeReducedForTop2] =
    Json.format[TimeReducedForTop2]

  implicit val formatDAmount: Format[DAmount] = Json.format[DAmount]
  implicit val formatDType: Format[DType] = Json.format[DType]
  implicit val formatDTarget: Format[DTarget] = Json.format[DTarget]
  implicit val sef: Format[SkillEffect] = Json.format[SkillEffect]

  // implicit val skillEffectWrites: Writes[SkillEffect] =
  //   new Writes[SkillEffect] {
  //     override def writes(o: SkillEffect): JsValue = o match {
  //       case _ => ???
  //     }
  //   }
  implicit val cardStatsFormat: Format[CardStats] = Json.format[CardStats]
  implicit val enemySkillFormat: Format[EnemySkill] = Json.format[EnemySkill]
  implicit val cardEnemySkillsFormat: Format[CardEnemySkills] =
    Json.format[CardEnemySkills]
  implicit val cardMiscStatsFormat: Format[CardMiscStats] =
    Json.format[CardMiscStats]

  implicit val cardWrites: Writes[Card] = (
    (JsPath \ "id").write[Long] and
      (JsPath \ "name").write[String] and
      (JsPath \ "att").write[Attribute] and
      (JsPath \ "subatt").write[Attribute] and
      (JsPath \ "types").write[List[CardType]] and
      (JsPath \ "activeSkill").write[ActiveSkill] and
      // (JsPath \ "leaderSkill").write[LeaderSkill] and
      (JsPath \ "awakenings").write[List[Awakening]] and
      (JsPath \ "superAwakenings").write[List[Awakening]] and
      (JsPath \ "isInheritable").write[Boolean] and
      (JsPath \ "isExtraSlottable").write[Boolean] and
      (JsPath \ "cardStats").write[CardStats] and
      (JsPath \ "cardEnemySkills").write[CardEnemySkills] and
      (JsPath \ "cardMiscStats").write[CardMiscStats]
  )((card: Card) =>
    (
      card.id,
      card.name,
      card.att,
      card.subatt,
      card.types,
      card.activeSkill,
      // card.leaderSkill,
      card.awakenings,
      card.superAwakenings,
      card.isInheritable,
      card.isExtraSlottable,
      card.cardStats,
      card.cardEnemySkills,
      card.cardMiscStats
    )
  )

  def jsonCardDataFromJson(data: List[JsValue]): JsonCardData = {
    val it = data.iterator
    val id = it.next().as[JsNumber].value.toLong
    val name = it.next().as[JsString].value // 1
    val att = it.next().as[JsNumber].value.toLong
    val subattribute = it.next().as[JsNumber].value.toLong // 3
    val isEvoReversable = it.next().as[JsNumber].value.toLong // 4
    val type1 = it.next().as[JsNumber].value.toLong // 5
    val type2 = it.next().as[JsNumber].value.toLong // 6
    val starCount = it.next().as[JsNumber].value.toLong // 7
    val cost = it.next().as[JsNumber].value.toLong // 8
    val unk9 = it.next().as[JsNumber].value.toLong // u0 ??? // 9
    val maxLevel = it.next().as[JsNumber].value.toLong // 10
    val feedExpPerLevel = it.next().as[JsNumber].value.toLong // 11
    val unk12 =
      it.next()
        .as[JsNumber]
        .value
        .toLong // u1 12 // ??? Seems to always be 100
    val sellPricePerLevel = it.next().as[JsNumber].value.toLong // 13
    val minHp = it.next().as[JsNumber].value.toLong // 14
    val maxHp = it.next().as[JsNumber].value.toLong // 15
    val unk16 =
      it.next()
        .as[JsNumber]
        .value
        .toLong // u2 16 // ??? May be HP multiplier related
    val minAtk = it.next().as[JsNumber].value.toLong // 17
    val maxAtk = it.next().as[JsNumber].value.toLong // 18
    val unk19 =
      it.next()
        .as[JsNumber]
        .value
        .toLong // u3 19 // ??? May be ATK multiplier related
    val minRcv = it.next().as[JsNumber].value.toLong // 20
    val maxRcv = it.next().as[JsNumber].value.toLong // 21
    val unk22 =
      it.next()
        .as[JsNumber]
        .value
        .toLong // u4 22 // ??? May be RCV multiplier related
    val expCurve = it.next().as[JsNumber].value.toLong // 23
    val unk24 =
      it.next().as[JsNumber].value.toDouble // u5 24 // ??? Mostly 2.5
    val activeSkillId = it.next().as[JsNumber].value.toInt // 25
    val leaderSkillId = it.next().as[JsNumber].value.toInt // 26
    val turnTimer = it.next().as[JsNumber].value.toLong // 27
    val enemyHpAtLv1 =
      it.next()
        .as[JsNumber]
        .value
        .toLong // u6 28 // ??? Probably related to HP in dungeons
    val enemyHpAtLv10 = it.next().as[JsNumber].value.toLong // u7 29 // ???
    val enemyHpCurve = it.next().as[JsNumber].value.toLong // u8 30 // ???
    val enemyAtkAtLv1 =
      it.next()
        .as[JsNumber]
        .value
        .toLong // u9 31 // ??? Probably related to ATK in dungeons
    val enemyAtkAtLv10 = it.next().as[JsNumber].value.toLong // u10 32 // ???
    val enemyAtkCurve = it.next().as[JsNumber].value.toLong // u11 33 // ???
    val enemyDefAtLv1 =
      it.next()
        .as[JsNumber]
        .value
        .toLong // u12 34 // ??? Probably related to DEF in dungeons
    val enemyDefAtLv10 = it.next().as[JsNumber].value.toLong // u13 35 // ???
    val enemyDefCurve = it.next().as[JsNumber].value.toLong // u14 36 // ???
    val maxEnemyLevel = it.next().as[JsNumber].value.toLong // 37
    val enemyCoinsAtLv2 = it.next().as[JsNumber].value.toLong // 38
    val enemyExpAtLv2 = it.next().as[JsNumber].value.toLong // 39
    val evoFromId = it.next().as[JsNumber].value.toLong // 40
    val evoMat1 = it.next().as[JsNumber].value.toLong // 41
    val evoMat2 = it.next().as[JsNumber].value.toLong // 42
    val evoMat3 = it.next().as[JsNumber].value.toLong // 43
    val evoMat4 = it.next().as[JsNumber].value.toLong // 44
    val evoMat5 = it.next().as[JsNumber].value.toLong // 45
    val devoMat1 = it.next().as[JsNumber].value.toLong // 46
    val devoMat2 = it.next().as[JsNumber].value.toLong // 47
    val devoMat3 = it.next().as[JsNumber].value.toLong // 48
    val devoMat4 = it.next().as[JsNumber].value.toLong // 49
    val devoMat5 = it.next().as[JsNumber].value.toLong // 50
    val altTurnTimer =
      it.next()
        .as[JsNumber]
        .value
        .toLong
    val aiType =
      it.next()
        .as[JsNumber]
        .value
        .toLong
    val charges = it.next().as[JsNumber].value.toLong // 53
    val chargeGain = it.next().as[JsNumber].value.toLong // 54
    val unk55 = it.next().as[JsNumber].value.toLong // 55 // ??? u7
    val unk56 = it.next().as[JsNumber].value.toLong // 56 // ??? u8
    val skillCount = it.next().as[JsNumber].value.toLong // 57
    val enemySkills: List[EnemySkillDataJson] = (1 to skillCount.toInt)
      .map(_ => {
        val skillId: Long = it.next().as[JsNumber].value.toLong
        val ai: Long = it.next().as[JsNumber].value.toLong
        val rnd: Long = it.next().as[JsNumber].value.toLong
        EnemySkillDataJson(skillId, ai, rnd)
      })
      .toList

    val awakeningCount = it.next().as[JsNumber].value.toLong

    val awakenings: List[Long] = (1 to awakeningCount.toInt)
      .map(_ => it.next().as[JsNumber].value.toLong)
      .toList
    val superAwakenings: String = it.next().as[JsString].value
    val evoTreeBaseId = it.next().as[JsNumber].value.toLong
    val internalGroup =
      it.next().as[JsNumber].value.toLong // lorewise internalGroup
    val type3 = it.next().as[JsNumber].value.toLong
    val monsterPoints = it.next().as[JsNumber].value.toLong
    val unk_e1 = it.next().as[JsNumber].value.toLong // ??? u24
    val collab = it.next().as[JsNumber].value.toLong
    val inheritanceType = it.next().as[JsNumber].value.toLong
    val searchGroup: String = it.next().as[JsString].value
    val limitBreakStatGain = it.next().as[JsNumber].value.toLong
    val voiceId = it.next().as[JsNumber].value.toLong
    val orbSkin = it.next().as[JsNumber].value.toLong
    JsonCardData(
      id = id,
      name = name,
      att = att,
      subattribute = subattribute,
      isEvoReversable = isEvoReversable,
      type1 = type1,
      type2 = type2,
      starCount = starCount,
      cost = cost,
      unk9 = unk9,
      maxLevel = maxLevel,
      feedExpPerLevel = feedExpPerLevel,
      unk12 = unk12,
      sellPricePerLevel = sellPricePerLevel,
      minHp = minHp,
      maxHp = maxHp,
      unk16 = unk16,
      minAtk = minAtk,
      maxAtk = maxAtk,
      unk19 = unk19,
      minRcv = minRcv,
      maxRcv = maxRcv,
      unk22 = unk22,
      expCurve = expCurve,
      unk24 = unk24,
      activeSkillId = activeSkillId,
      leaderSkillId = leaderSkillId,
      turnTimer = turnTimer,
      enemyHpAtLv1 = enemyHpAtLv1,
      enemyHpAtLv10 = enemyHpAtLv10,
      enemyHpCurve = enemyHpCurve,
      enemyAtkAtLv1 = enemyAtkAtLv1,
      enemyAtkAtLv10 = enemyAtkAtLv10,
      enemyAtkCurve = enemyAtkCurve,
      enemyDefAtLv1 = enemyDefAtLv1,
      enemyDefAtLv10 = enemyDefAtLv10,
      enemyDefCurve = enemyDefCurve,
      maxEnemyLevel = maxEnemyLevel,
      enemyCoinsAtLv2 = enemyCoinsAtLv2,
      enemyExpAtLv2 = enemyExpAtLv2,
      evoFromId = evoFromId,
      evoMat1 = evoMat1,
      evoMat2 = evoMat2,
      evoMat3 = evoMat3,
      evoMat4 = evoMat4,
      evoMat5 = evoMat5,
      devoMat1 = devoMat1,
      devoMat2 = devoMat2,
      devoMat3 = devoMat3,
      devoMat4 = devoMat4,
      devoMat5 = devoMat5,
      altTurnTimer = altTurnTimer,
      aiType = aiType,
      charges = charges,
      chargeGain = chargeGain,
      unk55 = unk55,
      unk56 = unk56,
      skillCount = skillCount,
      enemySkills = enemySkills,
      awakeningCount = awakeningCount,
      awakenings = awakenings,
      superAwakenings = superAwakenings,
      evoTreeBaseId = evoTreeBaseId,
      internalGroup = internalGroup,
      type3 = type3,
      monsterPoints = monsterPoints,
      unk_e1 = unk_e1,
      collab = collab,
      inheritanceType = inheritanceType,
      searchGroup = searchGroup,
      limitBreakStatGain = limitBreakStatGain,
      voiceId = voiceId,
      orbSkin = orbSkin
    )
  }

  def jsonSkillDataFromJson(data: List[JsValue], index: Int): JsonSkillData = {
    val it = data.iterator
    val id = index
    val name = it.next().as[JsString].value
    val desc = it.next().as[JsString].value
    val internalEffectId = it.next().as[JsNumber].value.toLong
    val maxLevel = it.next().as[JsNumber].value.toLong
    val initialCooldown = it.next().as[JsNumber].value.toLong
    // skill.maxCooldown = skill.initialCooldown - skill.maxLevel + 1;
    val unk1 = it.next().as[JsString].value
    val remaining = it.toList
    val args = remaining.map(_.as[JsNumber].value.toInt)
    JsonSkillData(
      id = id,
      name = name,
      desc = desc,
      internalEffectId = internalEffectId,
      maxLevel = maxLevel,
      initialCooldown = initialCooldown,
      unk1 = unk1,
      args = args
    )
  }

  def cardToJson(card: Card): String = {

    ???
  }

  def cardFromJson(json: String): Card = {
    ???
  }

  def cardsToJson(cards: Seq[Card]): String = {
    Json.toJson(cards).toString
  }

  def cardsFromJson(json: String): Array[Card] = { ??? }
}
