import scala.io.Source
import model._
import play.api.libs.json._
import skills.ActiveSkill
import skills.LeaderSkill
import scala.util.Random
val cardDataFile = "download_card_data.json"
val skillDataFile = "download_skill_data.json"
val cardData = readCardData(cardDataFile)
val skillData = readSkillData(skillDataFile)

val a = skillData
  .filter(_.internalEffectId == 136)
  .map(_.args)

def readCardData(path: String): Array[JsonCardData] = {
  val cardDataStr = Source.fromFile(path).mkString
  val json: JsValue = Json.parse(cardDataStr)
  val cards = (json \ "card")
    .as[JsArray]
    .value
    .map(card => {
      JsonCardData.fromJson(card.as[JsArray].value.toList)
    })
  cards.toArray
}

def readSkillData(path: String): Array[JsonSkillData] = {
  val cardDataStr = Source.fromFile(path).mkString
  val json: JsValue = Json.parse(cardDataStr)
  val skills = (json \ "skill")
    .as[JsArray]
    .value
    .zipWithIndex
    .map((card, ind) => {
      JsonSkillData.fromJson(card.as[JsArray].value.toList, ind)
    })
  skills.toArray
}
