import model.JsonCardData
import model.JsonSkillData
import scala.io.Source
import model.EnemySkillDataJson
import play.api.libs.json._
import model.Attribute
import skills.ActiveSkill

object DataReader {

  def main(arg: Array[String]): Unit = {
    val cardDataFile = "download_card_data.json"
    val skillDataFile = "download_skill_data.json"
    val cardData = readCardData(cardDataFile)
    val skillData = readSkillData(skillDataFile)
    println(skillData.length)
    println(cardData(7770))
    println()
    (0 to 235).map(i => {
      val card =
        cardData.tail.find(c =>
          skillData(c.activeSkillId).internalEffectId == i
        )
      if (card.isEmpty)
        println(card.getOrElse(s"skill id = ${i} No card found"))
      else {
        val theCard = card.get
        println(s"#${theCard.id}-${theCard.name}: skill id = ${i}")
        val skill =
          ActiveSkill.fromJson(theCard.activeSkillId, skillData, cardData)
        println(skill)
        println()
      }
    })
    println(skillData(23583))
    println(ActiveSkill.fromJson(23583, skillData, cardData))
  }

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

}
