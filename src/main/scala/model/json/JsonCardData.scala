package model

import play.api.libs.json.JsValue
import play.api.libs.json.JsNumber
import play.api.libs.json.JsString

final case class JsonCardData(
    id: Long, // 0
    name: String, // 1
    att: Long,
    subattribute: Long, // 3
    isEvoReversable: Long, // 4
    type1: Long, // 5
    type2: Long, // 6
    starCount: Long, // 7
    cost: Long, // 8
    unk9: Long, // u0 ??? // 9
    maxLevel: Long, // 10
    feedExpPerLevel: Long, // 11
    unk12: Long, // u1 12 // ??? Seems to always be 100
    sellPricePerLevel: Long, // 13
    minHp: Long, // 14
    maxHp: Long, // 15
    unk16: Long, // u2 16 // ??? May be HP multiplier related
    minAtk: Long, // 17
    maxAtk: Long, // 18
    unk19: Long, // u3 19 // ??? May be ATK multiplier related
    minRcv: Long, // 20
    maxRcv: Long, // 21
    unk22: Long, // u4 22 // ??? May be RCV multiplier related
    expCurve: Long, // 23
    unk24: Double, // u5 24 // ??? Mostly 2.5
    activeSkillId: Int, // 25
    leaderSkillId: Int, // 26
    turnTimer: Long, // 27
    enemyHpAtLv1: Long, // u6 28 // ??? Probably related to HP in dungeons
    enemyHpAtLv10: Long, // u7 29 // ???
    enemyHpCurve: Long, // u8 30 // ???
    enemyAtkAtLv1: Long, // u9 31 // ??? Probably related to ATK in dungeons
    enemyAtkAtLv10: Long, // u10 32 // ???
    enemyAtkCurve: Long, // u11 33 // ???
    enemyDefAtLv1: Long, // u12 34 // ??? Probably related to DEF in dungeons
    enemyDefAtLv10: Long, // u13 35 // ???
    enemyDefCurve: Long, // u14 36 // ???
    maxEnemyLevel: Long, // 37
    enemyCoinsAtLv2: Long, // 38
    enemyExpAtLv2: Long, // 39
    evoFromId: Long, // 40
    evoMat1: Long, // 41
    evoMat2: Long, // 42
    evoMat3: Long, // 43
    evoMat4: Long, // 44
    evoMat5: Long, // 45
    devoMat1: Long, // 46
    devoMat2: Long, // 47
    devoMat3: Long, // 48
    devoMat4: Long, // 49
    devoMat5: Long, // 50
    altTurnTimer: Long, // 51 // turn timer used in techs. If 0, uses regular turn timer
    aiType: Long, // 52 // 1 if it uses new ai, supporting some newer ES
    charges: Long, // 53
    chargeGain: Long, // 54
    unk55: Long, // 55 // ??? u7
    unk56: Long, // 56 // ??? u8
    skillCount: Long, // 57
    enemySkills: List[EnemySkillDataJson],
    awakeningCount: Long,
    awakenings: List[Long],
    superAwakenings: String,
    // superAwakenings: String,
    // if (superAwakenings !: "") {
    //     superAwakenings2 : superAwakenings.split(","),
    //     for (i : 0, i < superAwakenings2.length, i++) {
    //         superAwakening : superAwakenings2[i],
    //         superAwakenings.push(Awakening[Awakening[parseInt(superAwakening)]]),
    //     }
    // }
    evoTreeBaseId: Long,
    internalGroup: Long, // lorewise internalGroup
    type3: Long,
    monsterPoints: Long,
    unk_e1: Long, // ??? u24
    collab: Long,
    inheritanceType: Long,
    // isInheritable : ((inheritanceType & 1) :: 1),
    // extraSlottable : ((inheritanceType & 32) :: 32),
    searchGroup: String,
    limitBreakStatGain: Long,
    // isLimitBreakable : limitBreakStatGain > 0,
    voiceId: Long,
    orbSkin: Long
) {}

object JsonCardData {
  def fromJson(data: List[JsValue]): JsonCardData = {
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
}
