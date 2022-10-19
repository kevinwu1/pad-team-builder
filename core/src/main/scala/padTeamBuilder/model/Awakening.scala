package padTeamBuilder.model

enum Awakening(str: String) extends Enum[Awakening] {
  case None extends Awakening("None")
  case EnhancedHP extends Awakening("EnhancedHP")
  case EnhancedATK extends Awakening("EnhancedATK")
  case EnhancedRCV extends Awakening("EnhancedRCV")
  case FireResist extends Awakening("FireResist")
  case WaterResist extends Awakening("WaterResist")
  case WoodResist extends Awakening("WoodResist")
  case LightResist extends Awakening("LightResist")
  case DarkResist extends Awakening("DarkResist")
  case Autoheal extends Awakening("Autoheal")
  case BindResist extends Awakening("BindResist")
  case BlindResist extends Awakening("BlindResist")
  case JammerResist extends Awakening("JammerResist")
  case PoisonResist extends Awakening("PoisonResist")
  case FireEnhance extends Awakening("FireEnhance")
  case WaterEnhance extends Awakening("WaterEnhance")
  case WoodEnhance extends Awakening("WoodEnhance")
  case LightEnhance extends Awakening("LightEnhance")
  case DarkEnhance extends Awakening("DarkEnhance")
  case TimeExtend extends Awakening("TimeExtend")
  case BindRecovery extends Awakening("BindRecovery")
  case SkillBoost extends Awakening("SkillBoost")
  case FireRow extends Awakening("FireRow")
  case WaterRow extends Awakening("WaterRow")
  case WoodRow extends Awakening("WoodRow")
  case LightRow extends Awakening("LightRow")
  case DarkRow extends Awakening("DarkRow")
  case TPA extends Awakening("TPA")
  case SBR extends Awakening("SBR")
  case HeartEnhance extends Awakening("HeartEnhance")
  case Multiboost extends Awakening("Multiboost")
  case DragonKiller extends Awakening("DragonKiller")
  case GodKiller extends Awakening("GodKiller")
  case DevilKiller extends Awakening("DevilKiller")
  case MachineKiller extends Awakening("MachineKiller")
  case BalancedKiller extends Awakening("BalancedKiller")
  case AttackerKiller extends Awakening("AttackerKiller")
  case PhysicalKiller extends Awakening("PhysicalKiller")
  case HealerKiller extends Awakening("HealerKiller")
  case EvoKiller extends Awakening("EvoKiller")
  case AwakeningKiller extends Awakening("AwakeningKiller")
  case EnhanceKiller extends Awakening("EnhanceKiller")
  case RedeemableKiller extends Awakening("RedeemableKiller")
  case SevenCombo extends Awakening("SevenCombo")
  case Guardbreak extends Awakening("Guardbreak")
  case FUA extends Awakening("FUA")
  case TeamHP extends Awakening("TeamHP")
  case TeamRCV extends Awakening("TeamRCV")
  case VDP extends Awakening("VDP")
  case EquipAssist extends Awakening("EquipAssist")
  case SuperFUA extends Awakening("SuperFUA")
  case RainbowHaste extends Awakening("RainbowHaste")
  case UnbindablePlus extends Awakening("UnbindablePlus")
  case TimeExtendPlus extends Awakening("TimeExtendPlus")
  case CloudResist extends Awakening("CloudResist")
  case ScrollResist extends Awakening("ScrollResist")
  case SkillBoostPlus extends Awakening("SkillBoostPlus")
  case HP80 extends Awakening("HP80")
  case HP50 extends Awakening("HP50")
  case LShield extends Awakening("LShield")
  case LUnlock extends Awakening("LUnlock")
  case TenCombo extends Awakening("TenCombo")
  case ComboOrb extends Awakening("ComboOrb")
  case Voice extends Awakening("Voice")
  case Dungeon extends Awakening("Dungeon")
  case HpMinus extends Awakening("HpMinus")
  case AtkMinus extends Awakening("AtkMinus")
  case RcvMinus extends Awakening("RcvMinus")
  case BlindResistPlus extends Awakening("BlindResistPlus")
  case JammerResistPlus extends Awakening("JammerResistPlus")
  case PoisonResistPlus extends Awakening("PoisonResistPlus")
  case JammerSkyfall extends Awakening("JammerSkyfall")
  case PoisonSkyfall extends Awakening("PoisonSkyfall")
  case FireCombo extends Awakening("FireCombo")
  case WaterCombo extends Awakening("WaterCombo")
  case WoodCombo extends Awakening("WoodCombo")
  case LightCombo extends Awakening("LightCombo")
  case DarkCombo extends Awakening("DarkCombo")
  case CrossBoost extends Awakening("CrossBoost")
  case ThreeColour extends Awakening("ThreeColour")
  case FourColour extends Awakening("FourColour")
  case FiveColour extends Awakening("FiveColour")
  case Brick extends Awakening("Brick")
  case AddDragonType extends Awakening("AddDragonType")
  case AddGodType extends Awakening("AddGodType")
  case AddDevilType extends Awakening("AddDevilType")
  case AddMachineType extends Awakening("AddMachineType")
  case AddBalancedType extends Awakening("AddBalancedType")
  case AddAttackerType extends Awakening("AddAttackerType")
  case AddPhysicalType extends Awakening("AddPhysicalType")
  case AddHealerType extends Awakening("AddHealerType")
  case AddFireSubatt extends Awakening("AddFireSubatt")
  case AddWaterSubatt extends Awakening("AddWaterSubatt")
  case AddWoodSubatt extends Awakening("AddWoodSubatt")
  case AddLightSubatt extends Awakening("AddLightSubatt")
  case AddDarkSubatt extends Awakening("AddDarkSubatt")
  case TPAPlus extends Awakening("TPAPlus")
  case RainbowHastePlus extends Awakening("RainbowHastePlus")
  case AutohealPlus extends Awakening("AutohealPlus")
  case FireEnhancePlus extends Awakening("FireEnhancePlus")
  case WaterEnhancePlus extends Awakening("WaterEnhancePlus")
  case WoodEnhancePlus extends Awakening("WoodEnhancePlus")
  case LightEnhancePlus extends Awakening("LightEnhancePlus")
  case DarkEnhancePlus extends Awakening("DarkEnhancePlus")
  case HeartEnhancePlus extends Awakening("HeartEnhancePlus")
  case Unknown105 extends Awakening("Unknown105")
  case Unknown106 extends Awakening("Unknown106")
  case Unknown107 extends Awakening("Unknown107")
  case Unknown108 extends Awakening("Unknown108")
  case Unknown109 extends Awakening("Unknown109")
  case Unknown110 extends Awakening("Unknown110")
  case Unknown111 extends Awakening("Unknown111")
  case Unknown112 extends Awakening("Unknown112")
  case Unknown113 extends Awakening("Unknown113")
  case Unknown114 extends Awakening("Unknown114")
  case Super extends Awakening("Super")
  def atkMult = Awakening.getAtkMult(this)
  def rcvMult = Awakening.getRcvMult(this)
}

object Awakening {
  def from(awak: Int): Awakening = {
    if (awak == -1)
      Super
    else
      Awakening.fromOrdinal(awak)
  }

  def getAtkMult(awk: Awakening): Double = {
    awk match {
      case Multiboost | LShield                       => 1.5
      case TPA | LUnlock | HP80                       => 1.7
      case SevenCombo | JammerSkyfall | PoisonSkyfall => 2
      case HP50                                       => 2.2
      case VDP                                        => 2.5
      case DragonKiller | GodKiller | DevilKiller | MachineKiller |
          BalancedKiller | AttackerKiller | PhysicalKiller | HealerKiller |
          EvoKiller | AwakeningKiller | EnhanceKiller | RedeemableKiller =>
        3
      case CrossBoost  => 3
      case SuperFUA    => 3.5
      case TenCombo    => 5
      case ThreeColour => 2.5
      case FourColour  => 3.5
      case FiveColour  => 4.5
      case Brick       => 12
      case TPAPlus     => getAtkMult(Awakening.TPA) * getAtkMult(Awakening.TPA)
      case _           => 1
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
}
//hi <3
