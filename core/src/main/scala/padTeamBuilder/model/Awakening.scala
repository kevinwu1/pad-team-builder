package padTeamBuilder.model

enum Awakening extends Enum[Awakening] {
  case None
  case EnhancedHP
  case EnhancedATK
  case EnhancedRCV
  case FireResist
  case WaterResist
  case WoodResist
  case LightResist
  case DarkResist
  case Autoheal
  case Unbindable
  case BlindResist
  case JammerResist
  case PoisonResist
  case FireEnhance
  case WaterEnhance
  case WoodEnhance
  case LightEnhance
  case DarkEnhance
  case TimeExtend
  case BindRecovery
  case SkillBoost
  case FireRow
  case WaterRow
  case WoodRow
  case LightRow
  case DarkRow
  case TPA
  case SBR
  case HeartEnhance
  case Multiboost
  case DragonKiller
  case GodKiller
  case DevilKiller
  case MachineKiller
  case BalancedKiller
  case AttackerKiller
  case PhysicalKiller
  case HealerKiller
  case EvoKiller
  case AwakeningKiller
  case EnhanceKiller
  case RedeemableKiller
  case SevenCombo
  case Guardbreak
  case FUA
  case TeamHP
  case TeamRCV
  case VDP
  case EquipAssist
  case SuperFUA
  case RainbowHaste
  case UnbindablePlus
  case TimeExtendPlus
  case CloudResist
  case ScrollResist
  case SkillBoostPlus
  case HPHigh
  case HPLow
  case LShield
  case LUnlock
  case TenCombo
  case ComboOrb
  case Voice
  case Dungeon
  case HpMinus
  case AtkMinus
  case RcvMinus
  case BlindResistPlus
  case JammerResistPlus
  case PoisonResistPlus
  case JammerSkyfall
  case PoisonSkyfall
  case FireCombo
  case WaterCombo
  case WoodCombo
  case LightCombo
  case DarkCombo
  case CrossBoost
  case ThreeColour
  case FourColour
  case FiveColour
  case Brick
  case AddDragonType
  case AddGodType
  case AddDevilType
  case AddMachineType
  case AddBalancedType
  case AddAttackerType
  case AddPhysicalType
  case AddHealerType
  case AddFireSubatt
  case AddWaterSubatt
  case AddWoodSubatt
  case AddLightSubatt
  case AddDarkSubatt
  case TPAPlus
  case RainbowHastePlus
  case AutohealPlus
  case FireEnhancePlus
  case WaterEnhancePlus
  case WoodEnhancePlus
  case LightEnhancePlus
  case DarkEnhancePlus
  case HeartEnhancePlus
  case NegativeSkillBoost
  case Levitate
  case SevenComboPlus
  case LUnlockPlus
  case VDPPlus
  case CrossBoostPlus
  case TenComboPlus
  case ThreeColourPlus
  case FourColourPlus
  case FiveColourPlus
  case BindRecoveryPlus
  case FireRow3
  case WaterRow3
  case WoodRow3
  case LightRow3
  case DarkRow3
  case FireComboPlus
  case WaterComboPlus
  case WoodComboPlus
  case LightComboPlus
  case DarkComboPlus
  case TAwakening
  case Allstats
  case SunBoost
  case MoonBoost
  case Barrel
  case Super
  def str: String = toString()
  def atkMult = Awakening.getAtkMult(this)
  def rcvMult = Awakening.getRcvMult(this)
  infix def *(n: Int): List[Awakening] = List.fill(2)(this)
}

object Awakening {

  def main(args: Array[String]): Unit = {}

  def from(awak: Int): Awakening = {
    if (awak == -1)
      Super
    else
      Awakening.fromOrdinal(awak)
  }

  def getAtkMult(awk: Awakening): Double = {
    awk match {
      case Multiboost | LShield                       => 1.5
      case TPA                                        => 1.7
      case SevenCombo | JammerSkyfall | PoisonSkyfall => 2
      case LUnlock                                    => 2.2
      case HPHigh | HPLow                             => 2.5
      case ThreeColour                                => 2.5
      case CrossBoost                                 => 3
      case VDP                                        => 3.5
      case SuperFUA                                   => 3.5
      case FourColour                                 => 3.5
      case FiveColour                                 => 4.5
      case DragonKiller | GodKiller | DevilKiller | MachineKiller |
          BalancedKiller | AttackerKiller | PhysicalKiller | HealerKiller |
          EvoKiller | AwakeningKiller | EnhanceKiller | RedeemableKiller =>
        5
      case TenCombo  => 5
      case SunBoost  => 5
      case MoonBoost => 5
      case Brick     => 12
      case Levitate  => 20
      case other => {
        val expanded = expandCompactAwakenings(other)
        if (expanded.size > 1)
          expanded.map(getAtkMult).reduce(_ * _)
        else 1
      }
    }
  }

  def getRcvMult(awk: Awakening): Double = {
    awk match {
      case Multiboost   => 1.5
      case HeartEnhance => 1.5
      case HeartEnhancePlus =>
        getRcvMult(HeartEnhance) * getRcvMult(HeartEnhance)
      case _ => 1.0
    }
  }

  def expandCompactAwakenings(a: Awakening): List[Awakening] = {
    a match {
      case UnbindablePlus   => Unbindable * 2
      case TimeExtendPlus   => TimeExtend * 2
      case SkillBoostPlus   => SkillBoost * 2
      case BlindResistPlus  => BlindResist * 5
      case JammerResistPlus => JammerResist * 5
      case PoisonResistPlus => PoisonResist * 5
      case TPAPlus          => TPA * 2
      case RainbowHastePlus => RainbowHaste * 2
      case AutohealPlus     => Autoheal * 2
      case FireEnhancePlus  => FireEnhance * 2
      case WaterEnhancePlus => WaterEnhance * 2
      case WoodEnhancePlus  => WoodEnhance * 2
      case LightEnhancePlus => LightEnhance * 2
      case DarkEnhancePlus  => DarkEnhance * 2
      case HeartEnhancePlus => HeartEnhance * 2
      case SevenComboPlus   => SevenCombo * 2
      case LUnlockPlus      => LUnlock * 2
      case VDPPlus          => VDP * 2
      case CrossBoostPlus   => CrossBoost * 2
      case TenComboPlus     => TenCombo * 2
      case ThreeColourPlus  => ThreeColour * 2
      case FourColourPlus   => FourColour * 2
      case FiveColourPlus   => FiveColour * 2
      case BindRecoveryPlus => BindRecovery * 2
      case FireRow3         => FireRow * 3
      case WaterRow3        => WaterRow * 3
      case WoodRow3         => WoodRow * 3
      case LightRow3        => LightRow * 3
      case DarkRow3         => DarkRow * 3
      case FireComboPlus    => FireCombo * 2
      case WaterComboPlus   => WaterCombo * 2
      case WoodComboPlus    => WoodCombo * 2
      case LightComboPlus   => LightCombo * 2
      case DarkComboPlus    => DarkCombo * 2
      case _                => List(a)
    }
  }
}
//hi <3
