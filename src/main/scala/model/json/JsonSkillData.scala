package model

import play.api.libs.json.JsValue
import play.api.libs.json.JsString
import play.api.libs.json.JsNumber

final case class JsonSkillData(
    id: Int,
    name: String,
    desc: String,
    internalEffectId: Long,
    maxLevel: Long,
    initialCooldown: Long,
    // skill.maxCooldown = skill.initialCooldown - skill.maxLevel + 1;
    unk1: String,
    args: List[Int]
) {}

object JsonSkillData {
  def fromJson(data: List[JsValue], index: Int): JsonSkillData = {
    val it = data.iterator
    val id = index
    val name = it.next().as[JsString].value
    val desc = it.next().as[JsString].value
    val internalEffectId = it.next().as[JsNumber].value.toLong
    val maxLevel = it.next().as[JsNumber].value.toLong
    val initialCooldown = it.next().as[JsNumber].value.toLong
    // skill.maxCooldown = skill.initialCooldown - skill.maxLevel + 1;
    val unk1 = it.next().as[JsString].value
    val remaining = it.toList
    val args = remaining.map(_.as[JsNumber].value.toInt)
    JsonSkillData(
      id = id,
      name = name,
      desc = desc,
      internalEffectId = internalEffectId,
      maxLevel = maxLevel,
      initialCooldown = initialCooldown,
      unk1 = unk1,
      args = args
    )
  }

}
