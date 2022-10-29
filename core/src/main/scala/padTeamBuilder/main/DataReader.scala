package padTeamBuilder.main

import padTeamBuilder.json._
import padTeamBuilder.model._
import padTeamBuilder.skills.ActiveSkill
import padTeamBuilder.skills.LeaderSkill
import padTeamBuilder.skills.effects._
import padTeamBuilder.skills.effects.active._
import padTeamBuilder.skills.effects.leader.LSEffectNone
import play.api.libs.functional.syntax._
import play.api.libs.json._

import java.io.PrintStream
import scala.compiletime._
import scala.deriving.*
import scala.io.Source
import scala.util.Random

object DataReader {

  def main(arg: Array[String]): Unit = {
    println("3.5e4".toDouble)
  }

  def test() = {

    println(s"Current dir: ${System.getProperty("user.dir")}")
    val cardDataFile = "download_card_data.json"
    val skillDataFile = "download_skill_data.json"
    val cardData = readCardData(cardDataFile)
    val skillData = readSkillData(skillDataFile)
    println(skillData.length)
    println("carddata length: " + cardData.size)
    testLSParsing3(cardData, skillData)
    val cards = parseAllCardsFromJsonCardData(cardData, skillData)
  }
  def createParsedPickledCards() = {
    println(s"Current dir: ${System.getProperty("user.dir")}")
    val cardDataFile = "download_card_data.json"
    val skillDataFile = "download_skill_data.json"
    val cardData = readCardData(cardDataFile)
    val skillData = readSkillData(skillDataFile)
    println(skillData.length)
    println("carddata length: " + cardData.size)
    testLSParsing3(cardData, skillData)
    val cards = parseAllCardsFromJsonCardData(cardData, skillData)

    val (normalCards, altCards) = cards.partition(_.id < 20000)
    val desiredCards =
      normalCards.reverse.filter(_.name != "*****").filter(_.name != "????")
    val splits = 9
    val step = 1000
    (0 to 20).reverse
      .map(i =>
        (
          i,
          desiredCards.filter(c => i * step <= c.id && c.id < (i + 1) * step)
        )
      )
      .filter(_._2.nonEmpty)
      .foreach(t => {
        val ind = t._1
        val cards = t._2
        val path = s"parsed_cards_${ind}.pickle"
        writeAllCardsPickle(cards, path)
      })
  }
  def writeAllCardsPickle(cards: Vector[Card], path: String) = {
    println(s"wrting ${cards.size} cards")
    new PrintStream(path).print(Pickle.cardsToPickle(cards))
  }

  def createParsedCards() = {
    println(s"Current dir: ${System.getProperty("user.dir")}")
    val cardDataFile = "download_card_data.json"
    val skillDataFile = "download_skill_data.json"
    val cardData = readCardData(cardDataFile)
    val skillData = readSkillData(skillDataFile)
    println(skillData.length)
    println("carddata length: " + cardData.size)
    testLSParsing3(cardData, skillData)
    val cards = parseAllCardsFromJsonCardData(cardData, skillData)

    val (normalCards, altCards) = cards.partition(_.id < 20000)
    val basepath = "parsed_cards.json"
    val desiredCards =
      normalCards.reverse.filter(_.name != "*****").filter(_.name != "????")
    val splits = 9
    val step = 1000
    (0 to 20).reverse
      .map(i =>
        (
          i,
          desiredCards.filter(c => i * step <= c.id && c.id < (i + 1) * step)
        )
      )
      .filter(_._2.nonEmpty)
      .foreach(t => {
        val ind = t._1
        val cards = t._2
        val path = s"parsed_cards_${ind}.json"
        writeAllCards(cards, path)
      })
  }
  def writeAllCards(cards: Vector[Card], path: String) = {
    new PrintStream(path).print(JsonParsing.cardsToJson(cards))
  }

  def readAllCards(path: String): Vector[Card] = {
    val json = Source.fromFile(path).mkString
    JsonParsing.cardsFromJson(json)
  }

  def parseAllCardsFromJsonCardData(
      cardData: Vector[JsonCardData],
      skillData: Vector[JsonSkillData]
  ): Vector[Card] = {
    cardData.map(jcd => {
      Card.cardFromJsonCardData(jcd, skillData, cardData)
    })
  }

  def testLSParsing3(
      cardData: Vector[JsonCardData],
      skillData: Vector[JsonSkillData]
  ) = {
    (100 to 150).foreach(ind => {
      val theCard = cardData(Random(ind).between(0, cardData.size))
      val theskillData = skillData(theCard.leaderSkillId)
      val skill =
        LeaderSkill.fromJson(theCard.leaderSkillId, skillData, cardData)
      if (skill.effect != LSEffectNone) {
        println(s"index $ind")
        println(
          s"#${theCard.id}-${theCard.name}: LS id = ${theCard.leaderSkillId}: internal skill id = ${theskillData.internalEffectId}"
        )
        println("Desc: ")
        println(skillData(theCard.leaderSkillId).desc)
        println()
        println("Parsed: ")
        println(skill.effect)
        println()
        println()
        println()
      }
    })
  }

  def testLSParsing2(
      cardData: Vector[JsonCardData],
      skillData: Vector[JsonSkillData]
  ) = {
    val target = 221
    (0 to 9000).foreach(i => {
      val theCard = cardData(i)
      val theskillData = skillData(theCard.leaderSkillId)
      val matchesTarget =
        if (skillData(theCard.leaderSkillId).internalEffectId == 138)
          skillData(theCard.leaderSkillId).args
            .map(skillData)
            .map(_.internalEffectId)
            .exists(_ == target)
        else
          skillData(theCard.leaderSkillId).internalEffectId == target
      if (matchesTarget) {
        println(
          s"#${theCard.id}-${theCard.name}: LS id = ${theCard.leaderSkillId}: internal skill id = ${theskillData.internalEffectId}"
        )
        skillData(theCard.leaderSkillId).args
          .map(skillData)
          .filter(_.internalEffectId == target)
          .foreach(s =>
            println("${s.id} ::: ${s.internalEffectId} ::: ${s.args}")
          )
        println("Desc: ")
        println(skillData(theCard.leaderSkillId).desc)
        val skill =
          LeaderSkill.fromJson(theCard.leaderSkillId, skillData, cardData)
        println()
        println("Parsed: ")
        println(skill.effect)
        println()
        println()
        println()
      }
    })
  }

  def testLSParsing(
      cardData: Vector[JsonCardData],
      skillData: Vector[JsonSkillData]
  ) = {
    (8662 to 10000).foreach(i => {
      val theCard = cardData(i)
      val theskillData = skillData(theCard.leaderSkillId)
      println(
        s"#${theCard.id}-${theCard.name}: LS id = ${theCard.leaderSkillId}: internal skill id = ${theskillData.internalEffectId}"
      )
      println("Desc: ")
      println(skillData(theCard.leaderSkillId).desc)
      val skill =
        LeaderSkill.fromJson(theCard.leaderSkillId, skillData, cardData)
      println()
      println("Parsed: ")
      println(skill.effect)
      println()
      println()
      println()
    })
  }

  def testCardParsing(
      cardData: Vector[JsonCardData],
      skillData: Vector[JsonSkillData]
  ) = {
    (1 to 1000).foreach(i => {
      val jcd = cardData(i)
      val theskillData = skillData(jcd.activeSkillId)
      println("Parsing")
      val card = Card.cardFromJsonCardData(jcd, skillData, cardData)
      println("Parsed: ")
      println(card)
      println()
    })
  }

  def readCardData(path: String): Vector[JsonCardData] = {
    val cardDataStr = Source.fromFile("../" + path).mkString
    val json: JsValue = Json.parse(cardDataStr)
    val cards = (json \ "card")
      .as[JsArray]
      .value
      .map(card => {
        JsonParsing.jsonCardDataFromJson(card.as[JsArray].value.toList)
      })
    cards.toVector
  }

  def readSkillData(path: String): Vector[JsonSkillData] = {
    val cardDataStr = Source.fromFile("../" + path).mkString
    val json: JsValue = Json.parse(cardDataStr)
    val skills = (json \ "skill")
      .as[JsArray]
      .value
      .zipWithIndex
      .map((card, ind) => {
        JsonParsing.jsonSkillDataFromJson(card.as[JsArray].value.toList, ind)
      })
    skills.toVector
  }

  def findSkill(
      cardData: Vector[JsonCardData],
      skillData: Vector[JsonSkillData]
  ) = {
    (0 to 235).map(i => {
      val card =
        cardData.tail.find(c =>
          skillData(c.activeSkillId).internalEffectId == i
        )
      if (card.isEmpty)
        println(card.getOrElse(s"skill id = ${i} No card found"))
      else {
        val theCard = card.get
        println(s"#${theCard.id}-${theCard.name}: internal skill id = ${i}")
        val skill =
          ActiveSkill.fromJson(theCard.activeSkillId, skillData, cardData)
        println(skill)
        println("desc: ")
        println(skillData(theCard.activeSkillId).desc)
        println()
      }
    })
  }

  def testSkillParsing(
      cardData: Vector[JsonCardData],
      skillData: Vector[JsonSkillData]
  ) = {
    (3854 to 3854).foreach(i => {
      val theCard = cardData(i)
      val theskillData = skillData(theCard.activeSkillId)
      println(
        s"#${theCard.id}-${theCard.name}: skillid = ${theCard.activeSkillId}: internal skill id = ${theskillData.internalEffectId}"
      )
      println("Desc: ")
      println(skillData(theCard.activeSkillId).desc)
      val skill =
        ActiveSkill.fromJson(theCard.activeSkillId, skillData, cardData)
      println()
      println("Parsed: ")
      println(skill)
      println()
    })
  }
}
