package padTeamBuilder.model

enum Collab(str: String) extends Enum[Collab] {
  case None extends Collab("None")
  case Ragnarok extends Collab("Ragnarok")
  case Taiko extends Collab("Taiko")
  case ECO extends Collab("ECO")
  case UNKNOWN4 extends Collab("UNKNOWN4")
  case Gunma extends Collab("Gunma")
  case FFCD extends Collab("FFCD")
  case Necky extends Collab("Necky")
  case Punt extends Collab("Punt")
  case Android extends Collab("Android")
  case Shinrabansho extends Collab("Shinrabansho")
  case Kapibara extends Collab("Kapibara")
  case CrazyTower extends Collab("CrazyTower")
  case TenkaTrigger extends Collab("TenkaTrigger")
  case EVA extends Collab("EVA")
  case SevenEleven extends Collab("SevenEleven")
  case ClashOfClans extends Collab("ClashOfClans")
  case GrooveCoaster extends Collab("GrooveCoaster")
  case ROACE extends Collab("ROACE")
  case DragonsDogma extends Collab("DragonsDogma")
  case Takaoka extends Collab("Takaoka")
  case BattleCats extends Collab("BattleCats")
  case Batman extends Collab("Batman")
  case BaskinRobbins extends Collab("BaskinRobbins")
  case AngryBirds extends Collab("AngryBirds")
  case UNKNOWN25 extends Collab("UNKNOWN25")
  case HxH extends Collab("HxH")
  case HelloKitty extends Collab("HelloKitty")
  case PADBT extends Collab("PADBT")
  case BEAMS extends Collab("BEAMS")
  case Dragonball extends Collab("Dragonball")
  case SaintSeiya extends Collab("SaintSeiya")
  case RoadToDragons extends Collab("RoadToDragons")
  case DivineGate extends Collab("DivineGate")
  case SummonsBoard extends Collab("SummonsBoard")
  case Picotto extends Collab("Picotto")
  case Bikkuriman extends Collab("Bikkuriman")
  case AngryBirdsEpic extends Collab("AngryBirdsEpic")
  case DC extends Collab("DC")
  case Chibis1 extends Collab("Chibis1")
  case NorthStar extends Collab("NorthStar")
  case Chibis2 extends Collab("Chibis2")
  case UNKNOWN42 extends Collab("UNKNOWN42")
  case UNKNOWN43 extends Collab("UNKNOWN43")
  case Chibis3 extends Collab("Chibis3")
  case FinalFantasy extends Collab("FinalFantasy")
  case GhostInTheShell extends Collab("GhostInTheShell")
  case DuelMasters extends Collab("DuelMasters")
  case AttackOnTitan extends Collab("AttackOnTitan")
  case NinjaHattori extends Collab("NinjaHattori")
  case ShohenSunday extends Collab("ShohenSunday")
  case Crows extends Collab("Crows")
  case Bleach extends Collab("Bleach")
  case BatmanVSuperman extends Collab("BatmanVSuperman")
  case UNKNOWN54 extends Collab("UNKNOWN54")
  case PhoenixWright extends Collab("PhoenixWright")
  case Kenshin extends Collab("Kenshin")
  case Pepper extends Collab("Pepper")
  case Kinnikuman extends Collab("Kinnikuman")
  case NappingPrincess extends Collab("NappingPrincess")
  case Magazine extends Collab("Magazine")
  case MonsterHunter extends Collab("MonsterHunter")
  case CoroCoro extends Collab("CoroCoro")
  case Voltron extends Collab("Voltron")
  case DCUniverse extends Collab("DCUniverse")
  case FMA extends Collab("FMA")
  case KOF extends Collab("KOF")
  case YuYuHakusho extends Collab("YuYuHakusho")
  case Persona extends Collab("Persona")
  case CocaCola extends Collab("CocaCola")
  case MTG extends Collab("MTG")
  case ChronoMaGia extends Collab("ChronoMaGia")
  case SeventhRebirth extends Collab("SeventhRebirth")
  case CalcioFantasista extends Collab("CalcioFantasista")
  case PowerPro extends Collab("PowerPro")
  case Gintama extends Collab("Gintama")
  case SAO extends Collab("SAO")
  case KamenRider extends Collab("KamenRider")
  case YokaiWatch extends Collab("YokaiWatch")
  case Fate extends Collab("Fate")
  case StreetFighterV extends Collab("StreetFighterV")
  case Umaibo extends Collab("Umaibo")
  case McDonalds extends Collab("McDonalds")
  case ShamanKing extends Collab("ShamanKing")
  case Champion extends Collab("Champion")
  case Samsho extends Collab("Samsho")
  case PowerRangers extends Collab("PowerRangers")
  case Fujimi extends Collab("Fujimi")
  case Shinkalion extends Collab("Shinkalion")
  case YuGiOh extends Collab("YuGiOh")
  case Mickey extends Collab("Mickey")
  case DMC extends Collab("DMC")
  case MHA extends Collab("MHA")
  case DS extends Collab("DS")
  case Ninjala extends Collab("Ninjala")
  case Monogatari extends Collab("Monogatari")
  case Marvel extends Collab("Marvel")
  case JJK extends Collab("JJK")
  case DQ extends Collab("DQ")
  case Starwars extends Collab("Starwars")
  case Tennis extends Collab("Tennis")
  case OPM extends Collab("OPM")
  case Ultraman extends Collab("Ultraman")
  case UNKNOWN103 extends Collab("UNKNOWN103")
  case UNKNOWN104 extends Collab("UNKNOWN104")
  case UNKNOWN105 extends Collab("UNKNOWN105")
  case UNKNOWN106 extends Collab("UNKNOWN106")
  case Alts extends Collab("Alts")
  case DragonboundsDragoncallers extends Collab("DragonboundsDragoncallers")
}

object Collab {
  def from(collabNum: Int): Collab = {
    collabNum match {
      case 999   => Alts
      case 10001 => DragonboundsDragoncallers
      case _     => Collab.fromOrdinal(collabNum)
    }
  }
}
//hi <3
