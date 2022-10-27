package ptbFrontend

import padTeamBuilder.json._
import padTeamBuilder.model._
import padTeamBuilder.skills.ActiveSkill
import padTeamBuilder.skills.LeaderSkill
import padTeamBuilder.skills.effects._
import padTeamBuilder.skills.effects.active._
import padTeamBuilder.skills.effects.leader.LSEffectNone
import play.api.libs.functional.syntax._
import play.api.libs.json._

object TestingMain {
  def main(args: Array[String]) = {
    println(
      SkillSelector
        .ASMinMax(
          CounterAttackSkill(0, Attribute.NONE, 0),
          CounterAttackSkill(9999, Attribute.NONE, 9999)
        )
        .test(
          MultiEffect(
            List(
              CounterAttackSkill(3, Attribute.LIGHT, 4),
              OrbChangeAtoB(Attribute.FIRE, Attribute.WOOD)
            )
          )
        )
    )
  }
}
