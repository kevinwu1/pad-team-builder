package padTeamBuilder.json

import padTeamBuilder.model._
import padTeamBuilder.skills._
import padTeamBuilder.skills.effects.active._
import padTeamBuilder.skills.effects.leader._
import upickle.default._

object Pickle {

  implicit val AttributeReadWriter: ReadWriter[Attribute] =
    readwriter[Int].bimap[Attribute](_.ordinal, Attribute.fromOrdinal(_))
  implicit val CardTypeReadWriter: ReadWriter[CardType] =
    readwriter[Int].bimap[CardType](_.ordinal, CardType.fromOrdinal(_))
  implicit val AwakeningReadWriter: ReadWriter[Awakening] =
    readwriter[Int].bimap[Awakening](_.ordinal, Awakening.fromOrdinal(_))
  implicit val CardSlotReadWriter: ReadWriter[CardSlot] =
    readwriter[Int].bimap[CardSlot](_.ordinal, CardSlot.fromOrdinal(_))
  implicit val ColumnReadWriter: ReadWriter[Column] =
    readwriter[Int].bimap[Column](_.ordinal, Column.fromOrdinal(_))
  implicit val RowReadWriter: ReadWriter[Row] =
    readwriter[Int].bimap[Row](_.ordinal, Row.fromOrdinal(_))
  implicit val CollabReadWriter: ReadWriter[Collab] =
    readwriter[Int].bimap[Collab](_.ordinal, Collab.fromOrdinal(_))

  implicit val formatNoEffect: ReadWriter[NoEffect] = macroRW[NoEffect]
  implicit val formatMultiEffect: ReadWriter[MultiEffect] =
    macroRW[MultiEffect]
    // unreachable case showed up when i added ConditionalComponentTyped
  implicit val conditionalComponentFormat: ReadWriter[ConditionalComponent] =
    macroRW[ConditionalComponent]
  implicit val formatEvolvingEffect: ReadWriter[EvolvingEffect] =
    macroRW[EvolvingEffect]
  implicit val formatConditionalEffect: ReadWriter[ConditionalEffect] =
    macroRW[ConditionalEffect]
  implicit val formatChangeTheWorld: ReadWriter[ChangeTheWorld] =
    macroRW[ChangeTheWorld]
  implicit val formatCounterAttackSkill: ReadWriter[CounterAttackSkill] =
    macroRW[CounterAttackSkill]
  implicit val formatSuicide: ReadWriter[Suicide] =
    macroRW[Suicide]
  implicit val formatDefenseBreak: ReadWriter[DefenseBreak] =
    macroRW[DefenseBreak]
  implicit val formatDelay: ReadWriter[Delay] = macroRW[Delay]
  implicit val formatEnhanceOrbs: ReadWriter[EnhanceOrbs] = macroRW[EnhanceOrbs]
  implicit val formatGravityFalse: ReadWriter[GravityFalse] =
    macroRW[GravityFalse]
  implicit val formatGravityTrue: ReadWriter[GravityTrue] = macroRW[GravityTrue]
  implicit val formatHealFlat: ReadWriter[HealFlat] = macroRW[HealFlat]
  implicit val formatHealMultiplier: ReadWriter[HealMultiplier] =
    macroRW[HealMultiplier]
  implicit val formatHealPercentMax: ReadWriter[HealPercentMax] =
    macroRW[HealPercentMax]
  implicit val formatHealScalingByAwakening
      : ReadWriter[HealScalingByAwakening] =
    macroRW[HealScalingByAwakening]
  implicit val formatHealByTeamRCV: ReadWriter[HealByTeamRCV] =
    macroRW[HealByTeamRCV]
  implicit val formatHealPerTurn: ReadWriter[HealPerTurn] = macroRW[HealPerTurn]
  implicit val formatIncreaseSkyfall: ReadWriter[IncreaseSkyfall] =
    macroRW[IncreaseSkyfall]
  implicit val formatMassAttack: ReadWriter[MassAttack] = macroRW[MassAttack]
  implicit val formatOrbChangeAtoB: ReadWriter[OrbChangeAtoB] =
    macroRW[OrbChangeAtoB]
  implicit val formatOrbChangeFullBoard: ReadWriter[OrbChangeFullBoard] =
    macroRW[OrbChangeFullBoard]
  implicit val formatOrbChangeColumn: ReadWriter[OrbChangeColumn] =
    macroRW[OrbChangeColumn]
  implicit val formatOrbChangeColumnRandom: ReadWriter[OrbChangeColumnRandom] =
    macroRW[OrbChangeColumnRandom]
  implicit val formatOrbChangeRow: ReadWriter[OrbChangeRow] =
    macroRW[OrbChangeRow]
  implicit val formatOrbChangeRandomSpawn: ReadWriter[OrbChangeRandomSpawn] =
    macroRW[OrbChangeRandomSpawn]
  implicit val formatPoison: ReadWriter[Poison] = macroRW[Poison]
  implicit val formatRefresh: ReadWriter[Refresh] = macroRW[Refresh]
  implicit val formatRCVBoostMult: ReadWriter[RCVBoostMult] =
    macroRW[RCVBoostMult]
  implicit val formatRCVBoostByAwakening: ReadWriter[RCVBoostByAwakening] =
    macroRW[RCVBoostByAwakening]
  implicit val formatRCVBoostByAttributeAndType
      : ReadWriter[RCVBoostByAttributeAndType] =
    macroRW[RCVBoostByAttributeAndType]
  implicit val formatShieldAll: ReadWriter[ShieldAll] = macroRW[ShieldAll]
  implicit val formatShieldAttribute: ReadWriter[ShieldAttribute] =
    macroRW[ShieldAttribute]
  implicit val formatShieldScalingByAwakening
      : ReadWriter[ShieldScalingByAwakening] = macroRW[ShieldScalingByAwakening]
  implicit val formatSpikeAttribute: ReadWriter[SpikeAttribute] =
    macroRW[SpikeAttribute]
  implicit val formatSpikeType: ReadWriter[SpikeType] = macroRW[SpikeType]
  implicit val formatSpikeScalingByAwakening
      : ReadWriter[SpikeScalingByAwakening] =
    macroRW[SpikeScalingByAwakening]
  implicit val formatSpikeScalingByAttributeAndType
      : ReadWriter[SpikeScalingByAttributeAndType] =
    macroRW[SpikeScalingByAttributeAndType]
  implicit val formatSpikeSlots: ReadWriter[SpikeSlots] = macroRW[SpikeSlots]
  implicit val formatTransformFixed: ReadWriter[TransformFixed] =
    macroRW[TransformFixed]
  implicit val formatTransformRandom: ReadWriter[TransformRandom] =
    macroRW[TransformRandom]
  implicit val formatLeadSwap: ReadWriter[LeadSwapThisCard] =
    macroRW[LeadSwapThisCard]
  implicit val formatLeadSwapRightMost: ReadWriter[LeadSwapRightMost] =
    macroRW[LeadSwapRightMost]
  implicit val formatAwokenBindClear: ReadWriter[AwokenBindClear] =
    macroRW[AwokenBindClear]
  implicit val formatBindClear: ReadWriter[BindClear] = macroRW[BindClear]
  implicit val formatRandom: ReadWriter[Random] = macroRW[Random]
  implicit val formatTimeExtendFlat: ReadWriter[TimeExtendFlat] =
    macroRW[TimeExtendFlat]
  implicit val formatTimeExtendMult: ReadWriter[TimeExtendMult] =
    macroRW[TimeExtendMult]
  implicit val formatAttributeChange: ReadWriter[AttributeChangeSelf] =
    macroRW[AttributeChangeSelf]
  implicit val formatHasteFixed: ReadWriter[HasteFixed] = macroRW[HasteFixed]
  implicit val formatHasteRandom: ReadWriter[HasteRandom] = macroRW[HasteRandom]
  implicit val formatLockOrbs: ReadWriter[LockOrbs] = macroRW[LockOrbs]
  implicit val formatAttributeChangeEnemyPermanent
      : ReadWriter[AttributeChangeEnemyPermanent] =
    macroRW[AttributeChangeEnemyPermanent]
  implicit val formatAttributeChangeEnemyTemporary
      : ReadWriter[AttributeChangeEnemyTemporary] =
    macroRW[AttributeChangeEnemyTemporary]
  implicit val formatOrbChangeMultiTarget: ReadWriter[OrbChangeMultiTarget] =
    macroRW[OrbChangeMultiTarget]
  implicit val formatAddCombosSkill: ReadWriter[AddCombosSkill] =
    macroRW[AddCombosSkill]
  implicit val formatVoidDamageAbsorb: ReadWriter[VoidDamageAbsorb] =
    macroRW[VoidDamageAbsorb]
  implicit val formatVoidAttributeAbsorb: ReadWriter[VoidAttributeAbsorb] =
    macroRW[VoidAttributeAbsorb]
  implicit val formatVoidVoid: ReadWriter[VoidVoid] = macroRW[VoidVoid]
  implicit val formatOrbChangePattern: ReadWriter[OrbChangePattern] =
    macroRW[OrbChangePattern]
  implicit val formatEnhancedSkyfall: ReadWriter[EnhancedSkyfall] =
    macroRW[EnhancedSkyfall]
  implicit val formatOrbTrace: ReadWriter[OrbTrace] = macroRW[OrbTrace]
  implicit val formatAllyDelay: ReadWriter[AllyDelay] = macroRW[AllyDelay]
  implicit val formatAllyDelayRange: ReadWriter[AllyDelayRange] =
    macroRW[AllyDelayRange]
  implicit val formatUnlockOrbs: ReadWriter[UnlockOrbs] = macroRW[UnlockOrbs]
  implicit val formatUnmatchableClear: ReadWriter[UnmatchableClear] =
    macroRW[UnmatchableClear]
  implicit val formatNoSkyfallSkill: ReadWriter[NoSkyfallSkill] =
    macroRW[NoSkyfallSkill]
  implicit val formatConditionalComponentHP
      : ReadWriter[ConditionalComponentHP] =
    macroRW[ConditionalComponentHP]
  implicit val formatConditionalComponentFloor
      : ReadWriter[ConditionalComponentFloor] =
    macroRW[ConditionalComponentFloor]
  implicit val formatSpinnerRandom: ReadWriter[SpinnerRandom] =
    macroRW[SpinnerRandom]
  implicit val formatSpinnerFixed: ReadWriter[SpinnerFixed] =
    macroRW[SpinnerFixed]
  implicit val formatLockedSkyfall: ReadWriter[LockedSkyfall] =
    macroRW[LockedSkyfall]
  implicit val formatUnableToUseSkills: ReadWriter[UnableToUseSkills] =
    macroRW[UnableToUseSkills]
  implicit val formatSelfUnmatchable: ReadWriter[SelfUnmatchable] =
    macroRW[SelfUnmatchable]
  implicit val formatNailOrbSkyfall: ReadWriter[NailOrbSkyfall] =
    macroRW[NailOrbSkyfall]
  implicit val formatMaxHPMult: ReadWriter[MaxHPMult] = macroRW[MaxHPMult]
  implicit val formatImmediateDamage: ReadWriter[ImmediateDamage] =
    macroRW[ImmediateDamage]
  implicit val formatDFixed: ReadWriter[DFixed] = macroRW[DFixed]
  implicit val formatDMultiplier: ReadWriter[DMultiplier] = macroRW[DMultiplier]
  implicit val formatDRange: ReadWriter[DRange] = macroRW[DRange]
  implicit val formatDGrudge: ReadWriter[DGrudge] = macroRW[DGrudge]
  implicit val formatDTeamAtkMult: ReadWriter[DTeamAtkMult] =
    macroRW[DTeamAtkMult]
  implicit val formatDTeamHpMult: ReadWriter[DTeamHpMult] = macroRW[DTeamHpMult]
  implicit val formatDAttribute: ReadWriter[DAttribute] = macroRW[DAttribute]
  implicit val formatDTrue: ReadWriter[DTrue] = macroRW[DTrue]
  implicit val formatDInherit: ReadWriter[DInherit] = macroRW[DInherit]
  implicit val formatDSingle: ReadWriter[DSingle] = macroRW[DSingle]
  implicit val formatDAll: ReadWriter[DAll] = macroRW[DAll]
  implicit val formatDAttributeTarget: ReadWriter[DAttributeTarget] =
    macroRW[DAttributeTarget]
  implicit val formatTimeReducedForTop2: ReadWriter[TimeReducedForTop2] =
    macroRW[TimeReducedForTop2]

  implicit val formatDAmount: ReadWriter[DAmount] = ReadWriter.merge(
    formatDFixed,
    formatDMultiplier,
    formatDRange,
    formatDGrudge,
    formatDTeamAtkMult,
    formatDTeamHpMult
  )
  implicit val formatDType: ReadWriter[DType] = ReadWriter.merge(
    formatDAttribute,
    formatDTrue,
    formatDInherit
  )
  implicit val formatDTarget: ReadWriter[DTarget] = ReadWriter.merge(
    formatDSingle,
    formatDAll,
    formatDAttributeTarget
  )
  implicit val sef: ReadWriter[SkillEffect] = ReadWriter.merge(
    formatNoEffect,
    formatMultiEffect,
    formatEvolvingEffect,
    formatConditionalEffect,
    formatChangeTheWorld,
    formatCounterAttackSkill,
    formatSuicide,
    formatDefenseBreak,
    formatDelay,
    formatEnhanceOrbs,
    formatGravityFalse,
    formatGravityTrue,
    formatHealFlat,
    formatHealMultiplier,
    formatHealPercentMax,
    formatHealScalingByAwakening,
    formatHealByTeamRCV,
    formatHealPerTurn,
    formatIncreaseSkyfall,
    formatMassAttack,
    formatOrbChangeAtoB,
    formatOrbChangeFullBoard,
    formatOrbChangeColumn,
    formatOrbChangeColumnRandom,
    formatOrbChangeRow,
    formatOrbChangeRandomSpawn,
    formatPoison,
    formatRefresh,
    formatRCVBoostMult,
    formatRCVBoostByAwakening,
    formatRCVBoostByAttributeAndType,
    formatShieldAll,
    formatShieldAttribute,
    formatShieldScalingByAwakening,
    formatSpikeAttribute,
    formatSpikeType,
    formatSpikeScalingByAwakening,
    formatSpikeScalingByAttributeAndType,
    formatSpikeSlots,
    formatTransformFixed,
    formatTransformRandom,
    formatLeadSwap,
    formatLeadSwapRightMost,
    formatAwokenBindClear,
    formatBindClear,
    formatRandom,
    formatTimeExtendFlat,
    formatTimeExtendMult,
    formatAttributeChange,
    formatHasteFixed,
    formatHasteRandom,
    formatLockOrbs,
    formatAttributeChangeEnemyPermanent,
    formatAttributeChangeEnemyTemporary,
    formatOrbChangeMultiTarget,
    formatAddCombosSkill,
    formatVoidDamageAbsorb,
    formatVoidAttributeAbsorb,
    formatVoidVoid,
    formatOrbChangePattern,
    formatEnhancedSkyfall,
    formatOrbTrace,
    formatAllyDelay,
    formatAllyDelayRange,
    formatUnlockOrbs,
    formatUnmatchableClear,
    formatNoSkyfallSkill,
    formatSpinnerRandom,
    formatSpinnerFixed,
    formatLockedSkyfall,
    formatUnableToUseSkills,
    formatSelfUnmatchable,
    formatNailOrbSkyfall,
    formatMaxHPMult,
    formatImmediateDamage,
    formatTimeReducedForTop2
  )

  implicit val cardStatsFormat: ReadWriter[CardStats] = macroRW[CardStats]
  implicit val enemySkillFormat: ReadWriter[EnemySkill] = macroRW[EnemySkill]
  implicit val cardEnemySkillsFormat: ReadWriter[CardEnemySkills] =
    macroRW[CardEnemySkills]
  implicit val cardMiscStatsFormat: ReadWriter[CardMiscStats] =
    macroRW[CardMiscStats]
  implicit val activeSkillFormat: ReadWriter[ActiveSkill] = macroRW[ActiveSkill]
//leader skills

  implicit val formatLSCondition: ReadWriter[LSCondition] = ReadWriter.merge(
    formatLSConditionNone,
    formatLSConditionMulti,
    formatConditionAttribute,
    formatConditionType,
    formatConditionMatchingAnyOrbs,
    formatConditionHPThreshold,
    formatConditionColorsMatched,
    formatConditionCombos,
    formatConditionCombosExact,
    formatConditionOrbsLinked,
    formatConditionOrbsLinkedExact,
    formatConditionCardOnTeam,
    formatConditionSkillUsed,
    formatConditionSparkle,
    formatConditionCross,
    formatConditionCollab,
    formatConditionOrbsRemaining,
    formatConditionLMatched,
    formatConditionCoop,
    formatHealed,
    formatConditionAllPixel,
    formatConditionAllRevo,
    formatConditionTeamRarity
  )
  implicit val formatLSPayoff: ReadWriter[LSPayoff] = ReadWriter.merge(
    formatLSPayoffNone,
    formatHPBoost,
    formatATKBoost,
    formatRCVBoost,
    formatShieldElement,
    formatShieldRegular,
    formatBonusAttackScaling,
    formatBonusAttackFixed,
    formatAutoHealScaling,
    formatResolve,
    formatPayoffChance,
    formatDrummingSound,
    formatDropRateBoost,
    formatCoinBoost,
    formatCounterAttack,
    formatTimeExtend,
    formatRankExpBoost,
    formatNoSkyfall,
    formatBoard7x6,
    formatAddCombos,
    formatFixedTime,
    formatVoidPoisonDamage,
    formatMinimumMatch,
    formatPayoffAwokenBindClear,
    formatAddAwakening
  )

  implicit val formatLSPassive: ReadWriter[LSPassive] = macroRW[LSPassive]
  implicit val formatLSComponent: ReadWriter[LSComponent] = macroRW[LSComponent]
  implicit val formatLSComponentInfinite: ReadWriter[LSComponentInfinite] =
    macroRW[LSComponentInfinite]
  implicit val formatMultiLSEffect: ReadWriter[MultiLSEffect] =
    macroRW[MultiLSEffect]
  // implicit val formatLSEffectNone: ReadWriter[LSEffectNone] =
  //   readwriter[String].bimap[LSEffectNone](_ => "", _ => LSEffectNone())
  implicit val formatDynamicSelectEffect: ReadWriter[DynamicSelectEffect] =
    macroRW[DynamicSelectEffect]
  implicit val formatLSAttOrTypeScaling: ReadWriter[LSAttOrTypeScaling] =
    macroRW[LSAttOrTypeScaling]
  implicit val formatLSConditionNone: ReadWriter[LSConditionNone] =
    macroRW[LSConditionNone]
  implicit val formatLSConditionMulti: ReadWriter[LSConditionMulti] =
    macroRW[LSConditionMulti]
  implicit val formatConditionAttribute: ReadWriter[ConditionAttribute] =
    macroRW[ConditionAttribute]
  implicit val formatConditionType: ReadWriter[ConditionType] =
    macroRW[ConditionType]
  implicit val formatConditionMatchingAnyOrbs
      : ReadWriter[ConditionMatchingAnyOrbs] = macroRW[ConditionMatchingAnyOrbs]
  implicit val formatConditionHPThreshold: ReadWriter[ConditionHPThreshold] =
    macroRW[ConditionHPThreshold]
  implicit val formatConditionColorsMatched
      : ReadWriter[ConditionColorsMatched] =
    macroRW[ConditionColorsMatched]
  implicit val formatConditionCombos: ReadWriter[ConditionCombos] =
    macroRW[ConditionCombos]
  implicit val formatConditionCombosExact: ReadWriter[ConditionCombosExact] =
    macroRW[ConditionCombosExact]
  implicit val formatConditionOrbsLinked: ReadWriter[ConditionOrbsLinked] =
    macroRW[ConditionOrbsLinked]
  implicit val formatConditionOrbsLinkedExact
      : ReadWriter[ConditionOrbsLinkedExact] = macroRW[ConditionOrbsLinkedExact]
  implicit val formatConditionCardOnTeam: ReadWriter[ConditionCardOnTeam] =
    macroRW[ConditionCardOnTeam]
  implicit val formatConditionSkillUsed: ReadWriter[ConditionSkillUsed] =
    macroRW[ConditionSkillUsed]
  implicit val formatConditionSparkle: ReadWriter[ConditionSparkle] =
    macroRW[ConditionSparkle]
  implicit val formatConditionCross: ReadWriter[ConditionCross] =
    macroRW[ConditionCross]
  implicit val formatConditionCollab: ReadWriter[ConditionCollab] =
    macroRW[ConditionCollab]
  implicit val formatConditionOrbsRemaining
      : ReadWriter[ConditionOrbsRemaining] =
    macroRW[ConditionOrbsRemaining]
  implicit val formatConditionLMatched: ReadWriter[ConditionLMatched] =
    macroRW[ConditionLMatched]
  implicit val formatConditionCoop: ReadWriter[ConditionCoop] =
    macroRW[ConditionCoop]
  implicit val formatHealed: ReadWriter[Healed] = macroRW[Healed]
  implicit val formatConditionAllPixel: ReadWriter[ConditionAllPixel] =
    macroRW[ConditionAllPixel]
  implicit val formatConditionAllRevo: ReadWriter[ConditionAllRevo] =
    macroRW[ConditionAllRevo]
  implicit val formatConditionTeamRarity: ReadWriter[ConditionTeamRarity] =
    macroRW[ConditionTeamRarity]
  implicit val formatLSPayoffNone: ReadWriter[LSPayoffNone] =
    macroRW[LSPayoffNone]
  implicit val formatHPBoost: ReadWriter[HPBoost] = macroRW[HPBoost]
  implicit val formatATKBoost: ReadWriter[ATKBoost] = macroRW[ATKBoost]
  implicit val formatRCVBoost: ReadWriter[RCVBoost] = macroRW[RCVBoost]
  implicit val formatShieldElement: ReadWriter[ShieldElement] =
    macroRW[ShieldElement]
  implicit val formatShieldRegular: ReadWriter[ShieldRegular] =
    macroRW[ShieldRegular]
  implicit val formatBonusAttackScaling: ReadWriter[BonusAttackScaling] =
    macroRW[BonusAttackScaling]
  implicit val formatBonusAttackFixed: ReadWriter[BonusAttackFixed] =
    macroRW[BonusAttackFixed]
  implicit val formatAutoHealScaling: ReadWriter[AutoHealScaling] =
    macroRW[AutoHealScaling]
  implicit val formatResolve: ReadWriter[Resolve] = macroRW[Resolve]
  implicit val formatPayoffChance: ReadWriter[PayoffChance] =
    macroRW[PayoffChance]
  implicit val formatDrummingSound: ReadWriter[DrummingSound] =
    macroRW[DrummingSound]
  implicit val formatDropRateBoost: ReadWriter[DropRateBoost] =
    macroRW[DropRateBoost]
  implicit val formatCoinBoost: ReadWriter[CoinBoost] = macroRW[CoinBoost]
  implicit val formatCounterAttack: ReadWriter[CounterAttack] =
    macroRW[CounterAttack]
  implicit val formatTimeExtend: ReadWriter[TimeExtend] = macroRW[TimeExtend]
  implicit val formatRankExpBoost: ReadWriter[RankExpBoost] =
    macroRW[RankExpBoost]
  implicit val formatNoSkyfall: ReadWriter[NoSkyfall] = macroRW[NoSkyfall]
  implicit val formatBoard7x6: ReadWriter[Board7x6] = macroRW[Board7x6]
  implicit val formatAddCombos: ReadWriter[AddCombos] = macroRW[AddCombos]
  implicit val formatFixedTime: ReadWriter[FixedTime] = macroRW[FixedTime]
  implicit val formatVoidPoisonDamage: ReadWriter[VoidPoisonDamage] =
    macroRW[VoidPoisonDamage]
  implicit val formatMinimumMatch: ReadWriter[MinimumMatch] =
    macroRW[MinimumMatch]
  implicit val formatPayoffAwokenBindClear: ReadWriter[PayoffAwokenBindClear] =
    macroRW[PayoffAwokenBindClear]
  implicit val formatAddAwakening: ReadWriter[AddAwakening] =
    macroRW[AddAwakening]

  implicit val formatLSEffect: ReadWriter[LSEffect] =
    ReadWriter.merge(
      // formatLSEffectNone,
      formatLSPassive,
      formatLSComponent,
      formatLSComponentInfinite,
      formatMultiLSEffect,
      formatDynamicSelectEffect,
      formatLSAttOrTypeScaling
    )

  implicit val leaderSkillFormat: ReadWriter[LeaderSkill] = macroRW[LeaderSkill]

  implicit val cardRW: ReadWriter[Card] = macroRW[Card]

  def cardsToPickle(cards: Seq[Card]): String = {
    write(cards)
  }

  def cardsFromPickle(pickled: String): Vector[Card] = {
    read[Vector[Card]](pickled)
    // Json
    //   .parse(json)
    //   .as[JsArray]
    //   .value
    //   .map(card => {
    //     card.validate[Card] match {
    //       case s: JsSuccess[Card] => s.get
    //       case e: JsError => {
    //         System.err.println(e.errors)
    //         ???
    //       }
    //     }
    //   })
    //   .toVector
  }
}
