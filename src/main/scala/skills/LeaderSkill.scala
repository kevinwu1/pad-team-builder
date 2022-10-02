package model

import skills.effects._

case class LeaderSkill(name: String, effects: LSEffect) {}

object LeaderSkill {
  def fromJson(leaderSkill: Long, jsd: Seq[JsonSkillData]): LeaderSkill = {
    LeaderSkill("Temp", NoLSEffect)
  }
}
