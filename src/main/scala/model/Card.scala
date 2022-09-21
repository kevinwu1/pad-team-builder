package model

final case class Card(
    id: Long, // 0
    name: String, // 1
    att: Long,
    subattribute: Long, // 3
    isEvoReversable: Boolean, // 4
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
    activeSkillId: Long, // 25
    leaderSkillId: Long, // 26
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
