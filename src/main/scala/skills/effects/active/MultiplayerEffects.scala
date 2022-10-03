package skills.effects.active

trait MultiplayerEffect

case class TimeReducedForTop2(args: List[Int])
    extends SkillEffect(
      s"Orb move time halved for 1 turn for the top 2 ranked players"
    )
