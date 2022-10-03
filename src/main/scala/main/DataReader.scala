package main

import scala.io.Source
import model._
import play.api.libs.json._
import skills.ActiveSkill
import skills.LeaderSkill
import scala.util.Random

object DataReader {
  def main(arg: Array[String]): Unit = {
    val cardDataFile = "download_card_data.json"
    val skillDataFile = "download_skill_data.json"
    val cardData = readCardData(cardDataFile)
    val skillData = readSkillData(skillDataFile)
    println(skillData.length)
    println("carddata length: " + cardData.size)
    val cards = testLSParsing(cardData, skillData)
  }

  def parseAllCardsFromJsonCardData(
      cardData: Array[JsonCardData],
      skillData: Array[JsonSkillData]
  ) = {
    cardData.map(jcd => {
      Card.cardFromJsonCardData(jcd, skillData, cardData)
    })
  }

  def testLSParsing(
      cardData: Array[JsonCardData],
      skillData: Array[JsonSkillData]
  ) = {
    (0 to 250).foreach(i => {
      val card =
        cardData.tail
          .find(c => skillData(c.leaderSkillId).internalEffectId == i)
      if (card.isEmpty) println(card.getOrElse(s"LS id = ${i} No card found"))
      else {
        val theCard = card.get
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
      }
    })
  }

  def testCardParsing(
      cardData: Array[JsonCardData],
      skillData: Array[JsonSkillData]
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

  def findSkill(
      cardData: Array[JsonCardData],
      skillData: Array[JsonSkillData]
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
      cardData: Array[JsonCardData],
      skillData: Array[JsonSkillData]
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
