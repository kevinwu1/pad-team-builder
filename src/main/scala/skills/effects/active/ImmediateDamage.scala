package skills.effects.active

import model.Attribute

case class ImmediateDamage(
    amount: DAmount,
    damageType: DType,
    target: DTarget,
    drain: Option[Int] = None
) extends SkillEffect({
      val drainText =
        drain.map(p => s" and heal $p% of the damage").getOrElse("")
      s"Inflicts $amount $damageType on $target$drainText. "
    })

sealed trait DAmount(str: String) {
  override def toString = str
}
class DFixed(amount: Int) extends DAmount(amount.toString)
class DMultiplier(multiplier: Int) extends DAmount(s"${multiplier}x ATK")
class DRange(floor: Int, ceil: Int)
    extends DAmount(
      if (floor == ceil) DMultiplier(floor).toString
      else s"${floor}x-${ceil}x ATK"
    )
class DGrudge(hpMaxMult: Int, hp1Mult: Int)
    extends DAmount(s"[${hpMaxMult}x at full HP, up to ${hp1Mult}x at 1 HP]")
class DTeamAtkMult(multiplier: Int, atts: List[Attribute])
    extends DAmount(
      s"[${multiplier}x of entire team's ${atts.mkString(" and ")} ATK]"
    )
class DTeamHpMult(multiplier: Int)
    extends DAmount(
      s"${multiplier}x of entire team's HP"
    )

sealed trait DType(str: String) {
  override def toString = str
}
class DAttribute(attribute: Attribute)
    extends DType(attribute.toString + " damage")
object DTrue extends DType("true damage")
object DInherit extends DType("damage")

sealed trait DTarget(str: String) {
  override def toString = str
}
object DSingle extends DTarget(s"1 enemy")
object DAll extends DTarget(s"all enemies")
class DAttributeTarget(attribute: Attribute)
    extends DTarget(s"$attribute enemies")
