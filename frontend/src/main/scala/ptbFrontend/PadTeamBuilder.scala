package ptbFrontend

import com.raquo.airstream.core.Observer
import com.raquo.airstream.web._
import com.raquo.laminar.api.L._
import org.scalajs.dom
import org.scalajs.dom._
import padTeamBuilder.model._
import padTeamBuilder.skills._
import padTeamBuilder.skills.effects.active._
import padTeamBuilder.skills.effects.leader._
import play.api.libs.json._

import CardResults.CardSearchResult
import SkillSelector._

object PadTeamBuilder {

  val cards: Var[Vector[Card]] = Var(Vector[Card]())

  Util.loadCards(cards)

  val selectedAwakenings: Var[List[Awakening]] = Var(List())

  val asExpression: Var[ASFilter] = Var({
    ASSelect(None, "")
  })

  val filteredCards: Signal[Vector[CardSearchResult]] =
    selectedAwakenings.signal
      .combineWith(cards.signal)
      .combineWith(asExpression.signal)
      .mapN((awaks, cards, asExpression) => {
        val r = cards
          .map(card => {
            val (hasRequiredAwakenings, supers) =
              card.containsAwakenings(awaks, true)
            (card, hasRequiredAwakenings, supers)
          })
          .filter(_._2)
          .filter(t => asExpression.test(t._1.activeSkill.skillEffect))
          .map(t => (t._1, t._3))
        r
      })

  def main(args: Array[String]): Unit = {
    val rootElement: HtmlElement = div(
      AwakeningSelector.renderAwakeningSelector(selectedAwakenings),
      SkillSelector.renderSkillSelector(asExpression),
      CardResults.renderCardResults(filteredCards)
    )
    val containerNode = dom.document.querySelector("#root")
    render(containerNode, rootElement)
  }

}
