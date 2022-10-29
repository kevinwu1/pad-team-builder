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

import scala.collection.mutable.PriorityQueue
import scala.util.Try

import CardResults.{
  CardSearchResult,
  RankedCardSearchResult,
  CardRanker,
  isDesc,
  resultsRanker,
  RESULTS_MAX
}
import SkillSelector._

object PadTeamBuilder {

  val cards: Signal[Vector[Card]] = Util.loadCards()

  val selectedAwakenings: Var[List[Awakening]] = Var(List())

  val asExpression: Var[ASFilter] = Var({
    ASSelect(None, "")
  })

  def resultFromComponents(
      card: Card,
      awaks: List[Awakening],
      asExpression: ASFilter,
      ranker: CardRanker
  ): Option[RankedCardSearchResult] = {
    val (hasRequiredAwakenings, supers) =
      card.containsAwakenings(awaks, true)
    if (
      hasRequiredAwakenings && asExpression
        .test(card.activeSkill.skillEffect)
    ) {
      val csr = (card, supers)
      Some((csr, ranker(csr)))
    } else None
  }

  def orderingFromRanker(ranker: CardRanker, isDesc: Boolean) =
    new Ordering[RankedCardSearchResult]() {
      override def compare(
          x: RankedCardSearchResult,
          y: RankedCardSearchResult
      ): Int =
        if (isDesc)
          y._2.compareTo(x._2)
        else
          x._2.compareTo(y._2)
    }
  class Wrapper(p: PriorityQueue[RankedCardSearchResult]) {
    def get = p
    def update = new Wrapper(p)
  }

  val cardResults: Var[Vector[RankedCardSearchResult]] = Var(Vector())

  cards.signal
    .combineWith(selectedAwakenings.signal)
    .combineWith(asExpression.signal)
    .combineWith(resultsRanker.signal)
    .combineWith(isDesc.signal)
    .addObserver(Observer { nextValue =>
      val cards = nextValue._1
      val awaks = nextValue._2
      val asExpression = nextValue._3
      val ranker = nextValue._4
      val isDesc = nextValue._5
      println(s"Setting, desc: $isDesc")
      val ordering = orderingFromRanker(ranker, isDesc)

      val pq =
        PriorityQueue()(orderingFromRanker(resultsRanker.now(), isDesc))

      cards.foreach(card => {
        resultFromComponents(card, awaks, asExpression, ranker).foreach(
          result => {
            pq.addOne(result)
            if (pq.size > RESULTS_MAX) {
              pq.dequeue()
            }
          }
        )
      })
      cardResults.update(w => {
        pq.dequeueAll.toVector.reverse
      })
    })(unsafeWindowOwner)

  def main(args: Array[String]): Unit = {
    val rootElement: HtmlElement = div(
      AwakeningSelector.renderAwakeningSelector(selectedAwakenings),
      SkillSelector.renderSkillSelector(asExpression),
      CardResults.renderCardResults(
        cardResults.signal
      )
    )
    val containerNode = dom.document.querySelector("#root")
    render(containerNode, rootElement)
  }

}
