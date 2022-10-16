package ptbFrontend

import com.raquo.laminar.api.L._
import org.scalajs.dom
import org.scalajs.dom._
import padTeamBuilder.model._
import padTeamBuilder.skills._
import padTeamBuilder.skills.effects.active._
import padTeamBuilder.skills.effects.leader._
import com.raquo.airstream.web._
import CardResults.CardSearchResult

import play.api.libs.json._
import com.raquo.airstream.core.Observer

// the name 'Laminar101' matches the 'main' method setting in the
// build.sbt file (along with the package name 'alvin').
object PadTeamBuilder {

  val cards: Var[Vector[Card]] = Var(Vector[Card]())

  Util.loadCards(cards)

  val selectedAwakenings: Var[List[Awakening]] = Var(List())

  val filteredCards: Signal[Vector[CardSearchResult]] =
    selectedAwakenings.signal
      .combineWith(cards.signal)
      .mapN((awaks, cards) => {
        val r = cards
          .map(card => {
            val (has, supers) = card.containsAwakenings(awaks, true)
            (card, has, supers)
          })
          .filter(_._2)
          .map(t => (t._1, t._3))
        r.slice(0, 200)
      })

  def main(args: Array[String]): Unit = {
    val rootElement: HtmlElement = div(
      AwakeningSelector.renderAwakeningSelector(selectedAwakenings),
      CardResults.renderCardResults(filteredCards)
    )
    val containerNode = dom.document.querySelector("#root")
    render(containerNode, rootElement)
  }
}
