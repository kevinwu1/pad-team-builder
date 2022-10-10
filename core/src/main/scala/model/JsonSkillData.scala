package padTeamBuilder.model

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
