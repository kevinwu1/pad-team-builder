package padTeamBuilder.skills.effects.leader
import padTeamBuilder.model._

sealed trait LSPayoff(str: String) {
  override def toString = str
}

object LSPayoffNone extends LSPayoff("no effect")
sealed trait StatBoost extends LSPayoff
case class HPBoost(mult: Double)
    extends StatBoost
    with LSPayoff(
      s"${mult}x HP"
    )
case class ATKBoost(mult: Double)
    extends StatBoost
    with LSPayoff(
      s"${mult}x ATK"
    )
case class RCVBoost(mult: Double)
    extends StatBoost
    with LSPayoff(
      s"${mult}x RCV"
    )

sealed trait Shield

case class ShieldElement(reduction: Int, att: Attribute)
    extends Shield
    with LSPayoff(
      s"$reduction% reduced $att damage taken"
    )

case class ShieldRegular(reduction: Int)
    extends Shield
    with LSPayoff(
      s"$reduction% reduced damage taken"
    )

sealed trait BonusAttack
case class BonusAttackScaling(ratio: Double)
    extends BonusAttack
    with LSPayoff(
      s"inflicts a bonus attack equal to ${ratio}x ATK"
    )

case class BonusAttackFixed(dmg: Int)
    extends BonusAttack
    with LSPayoff(
      s"inflicts a $dmg damage bonus attack"
    )

trait AutoHeal
case class AutoHealScaling(ratio: Double)
    extends AutoHeal
    with LSPayoff(
      s"heal HP equal to ${ratio}x RCV"
    )

object Resolve
    extends LSPayoff(
      s"survive a single hit"
    )

case class PayoffChance(chance: Int, payoff: LSPayoff)
    extends LSPayoff(
      s"$chance% chance for $payoff"
    )

def Chance(chance: Int, payoff: LSPayoff): LSPayoff = {
  if (chance != 100)
    PayoffChance(chance, payoff)
  else payoff
}

object DrummingSound
    extends LSPayoff(
      s"A drumming sound is made when orbs are moved"
    )

case class DropRateBoost(rate: Double)
    extends LSPayoff(
      s"${rate}x coin drop rate when entering a dungeon as Leader"
    )

case class CoinBoost(rate: Double)
    extends LSPayoff(
      s"${rate}x coin drop rate when entering a dungeon as Leader"
    )

case class CounterAttack(mult: Double, att: Attribute)
    extends LSPayoff(
      s"${mult}x $att counterattack"
    )

case class TimeExtend(seconds: Double)
    extends LSPayoff(
      s"${if (seconds > 0) "+" else ""}$seconds seconds move time"
    )

case class RankExpBoost(rate: Double)
    extends LSPayoff(
      s"${rate}x rank EXP"
    )

object NoSkyfall
    extends LSPayoff(
      s"No skyfall"
    )

object Board7x6
    extends LSPayoff(
      s"7x6 board"
    )

case class AddCombos(numCombos: Int)
    extends LSPayoff(
      s"adds $numCombos combo${if (numCombos == 1) "" else "s"}"
    )

case class FixedTime(seconds: Double)
    extends LSPayoff(
      s"fixes move time at $seconds seconds"
    )

object VoidPoisonDamage
    extends LSPayoff(
      s"voids poison damage"
    )

case class MinimumMatch(min: Int)
    extends LSPayoff(
      s"cannot clear matches of ${min - 1} or fewer orbs"
    )

case class PayoffAwokenBindClear(turns: Int)
    extends LSPayoff(
      s"awoken bind reduce by $turns turns"
    )

case class AddAwakening(awk: Awakening)
    extends LSPayoff(
      s"adds $awk awakening"
    )
