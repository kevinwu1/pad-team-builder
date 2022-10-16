package padTeamBuilder.model

import padTeamBuilder.json._

import padTeamBuilder.skills.ActiveSkill
import padTeamBuilder.skills.LeaderSkill

final case class Card(
    id: Long,
    name: String,
    att: Attribute,
    subatt: Attribute,
    types: List[CardType],
    activeSkill: ActiveSkill,
    leaderSkill: LeaderSkill,
    awakenings: List[Awakening],
    superAwakenings: List[Awakening],
    isInheritable: Boolean,
    isExtraSlottable: Boolean,
    cardStats: CardStats,
    cardEnemySkills: CardEnemySkills,
    cardMiscStats: CardMiscStats
) {
  def containsAwakenings(
      awks: Seq[Awakening],
      includeSupers: Boolean
  ): (Boolean, List[Awakening]) = {
    val diff = awks.diff(awakenings)
    if (!includeSupers) (diff.size == 0, List())
    else {
      if (diff.size == 0) {
        (true, superAwakenings)
      } else if (diff.size == 1 && superAwakenings.contains(diff.head)) {
        (true, diff.toList)
      } else {
        (false, superAwakenings)
      }
    }
  }

}

case class CardStats(
    starCount: Long,
    cost: Long,
    maxLevel: Long,
    minHp: Long,
    maxHp: Long,
    minAtk: Long,
    maxAtk: Long,
    minRcv: Long,
    maxRcv: Long,
    limitBreakStatGain: Long,
    isLimitBreakable: Boolean
) {
  lazy val finalHp: Long =
    (maxHp * (100 + limitBreakStatGain + 10) / 100.0 + 99 * 10).round
  lazy val finalAtk: Long =
    (maxAtk * (100 + limitBreakStatGain + 5) / 100.0 + 99 * 5).round
  lazy val finalRcv: Long =
    (maxRcv * (100 + limitBreakStatGain + 5) / 100.0 + 99 * 3).round
}

case class CardEnemySkills(
    turnTimer: Long,
    enemyHpAtLv1: Long,
    enemyHpAtLv10: Long,
    enemyHpCurve: Long,
    enemyAtkAtLv1: Long,
    enemyAtkAtLv10: Long,
    enemyAtkCurve: Long,
    enemyDefAtLv1: Long,
    enemyDefAtLv10: Long,
    enemyDefCurve: Long,
    maxEnemyLevel: Long,
    enemyCoinsAtLv2: Long,
    enemyExpAtLv2: Long,
    altTurnTimer: Long,
    aiType: Long,
    charges: Long,
    chargeGain: Long,
    skillCount: Long,
    enemySkills: List[EnemySkill]
)

case class CardMiscStats(
    isEvoReversable: Boolean,
    expCurve: Long,
    feedExpPerLevel: Long,
    sellPricePerLevel: Long,
    evoFromId: Long,
    evoMat1: Long,
    evoMat2: Long,
    evoMat3: Long,
    evoMat4: Long,
    evoMat5: Long,
    devoMat1: Long,
    devoMat2: Long,
    devoMat3: Long,
    devoMat4: Long,
    devoMat5: Long,
    evoTreeBaseId: Long,
    collab: Long,
    internalGroup: Long,
    monsterPoints: Long,
    searchGroup: String,
    voiceId: Long,
    orbSkin: Long
)

object Card {

  def cardFromJsonCardData(
      jcd: JsonCardData,
      skillData: Seq[JsonSkillData],
      cardData: Seq[JsonCardData]
  ): Card = {
    val id: Long = jcd.id
    val name: String = jcd.name
    val att: Attribute = Attribute.from(jcd.att.toInt)
    val subatt: Attribute = Attribute.from(jcd.subattribute.toInt)
    val isEvoReversable: Boolean = jcd.isEvoReversable == 1
    val types: List[CardType] =
      List(jcd.type1, jcd.type2, jcd.type3)
        .map(t => CardType.from(t.toInt))
        .filter(_ != CardType.NoType)
    val starCount: Int = jcd.starCount.toInt
    val cost: Int = jcd.cost.toInt
    val maxLevel: Long = jcd.maxLevel
    val feedExpPerLevel: Long = jcd.feedExpPerLevel
    val sellPricePerLevel: Long = jcd.sellPricePerLevel
    val minHp: Long = jcd.minHp
    val maxHp: Long = jcd.maxHp
    val minAtk: Long = jcd.minAtk
    val maxAtk: Long = jcd.maxAtk
    val minRcv: Long = jcd.minRcv
    val maxRcv: Long = jcd.maxRcv
    val expCurve: Long = jcd.expCurve
    val activeSkill: ActiveSkill =
      ActiveSkill.fromJson(jcd.activeSkillId, skillData, cardData)
    val leaderSkill: LeaderSkill =
      LeaderSkill.fromJson(jcd.leaderSkillId, skillData, cardData)
    val turnTimer: Int = jcd.turnTimer.toInt
    val enemyHpAtLv1: Long = jcd.enemyHpAtLv1
    val enemyHpAtLv10: Long = jcd.enemyHpAtLv10
    val enemyHpCurve: Long = jcd.enemyHpCurve
    val enemyAtkAtLv1: Long = jcd.enemyAtkAtLv1
    val enemyAtkAtLv10: Long = jcd.enemyAtkAtLv10
    val enemyAtkCurve: Long = jcd.enemyAtkCurve
    val enemyDefAtLv1: Long = jcd.enemyDefAtLv1
    val enemyDefAtLv10: Long = jcd.enemyDefAtLv10
    val enemyDefCurve: Long = jcd.enemyDefCurve
    val maxEnemyLevel: Long = jcd.maxEnemyLevel
    val enemyCoinsAtLv2: Long = jcd.enemyCoinsAtLv2
    val enemyExpAtLv2: Long = jcd.enemyExpAtLv2
    val evoFromId: Long = jcd.evoFromId
    val evoMat1: Long = jcd.evoMat1
    val evoMat2: Long = jcd.evoMat2
    val evoMat3: Long = jcd.evoMat3
    val evoMat4: Long = jcd.evoMat4
    val evoMat5: Long = jcd.evoMat5
    val devoMat1: Long = jcd.devoMat1
    val devoMat2: Long = jcd.devoMat2
    val devoMat3: Long = jcd.devoMat3
    val devoMat4: Long = jcd.devoMat4
    val devoMat5: Long = jcd.devoMat5
    val altTurnTimer: Int = jcd.altTurnTimer.toInt
    val aiType: Long = jcd.aiType
    val charges: Long = jcd.charges
    val chargeGain: Long = jcd.chargeGain
    val skillCount: Long = jcd.skillCount
    val enemySkills: List[EnemySkill] = List() // jcd.enemySkills
    val awakeningCount: Long = jcd.awakeningCount
    val awakenings: List[Awakening] =
      jcd.awakenings.map(_.toInt).map(Awakening.from)
    val superAwakenings: List[Awakening] =
      jcd.superAwakenings
        .split(",")
        .filter(_.nonEmpty)
        .map(_.toInt)
        .map(Awakening.from)
        .toList
    val evoTreeBaseId: Long = jcd.evoTreeBaseId
    val internalGroup: Long = jcd.internalGroup
    val monsterPoints: Long = jcd.monsterPoints
    val collab: Long = jcd.collab
    val isInheritable: Boolean = (jcd.inheritanceType & 1) == 1
    val isExtraSlottable: Boolean = (jcd.inheritanceType & 32) == 32
    val searchGroup: String = jcd.searchGroup
    val limitBreakStatGain: Long = jcd.limitBreakStatGain
    val isLimitBreakable: Boolean = jcd.limitBreakStatGain > 0
    val voiceId: Long = jcd.voiceId
    val orbSkin: Long = jcd.orbSkin
    Card(
      id = id,
      name = name,
      att = att,
      subatt = subatt,
      types = types,
      activeSkill = activeSkill,
      leaderSkill = leaderSkill,
      awakenings = awakenings,
      superAwakenings = superAwakenings,
      isInheritable = isInheritable,
      isExtraSlottable = isExtraSlottable,
      cardStats = CardStats(
        starCount = starCount,
        cost = cost,
        maxLevel = maxLevel,
        minHp = minHp,
        maxHp = maxHp,
        minAtk = minAtk,
        maxAtk = maxAtk,
        minRcv = minRcv,
        maxRcv = maxRcv,
        limitBreakStatGain = limitBreakStatGain,
        isLimitBreakable = isLimitBreakable
      ),
      cardEnemySkills = CardEnemySkills(
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
        altTurnTimer = altTurnTimer,
        aiType = aiType,
        charges = charges,
        chargeGain = chargeGain,
        skillCount = skillCount,
        enemySkills = enemySkills
      ),
      cardMiscStats = CardMiscStats(
        isEvoReversable = isEvoReversable,
        expCurve = expCurve,
        feedExpPerLevel = feedExpPerLevel,
        sellPricePerLevel = sellPricePerLevel,
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
        evoTreeBaseId = evoTreeBaseId,
        internalGroup = internalGroup,
        monsterPoints = monsterPoints,
        collab = collab,
        searchGroup = searchGroup,
        voiceId = voiceId,
        orbSkin = orbSkin
      )
    )
  }
}
