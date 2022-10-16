package ptbFrontend

import com.raquo.airstream.web._
import com.raquo.laminar.api.L._
import padTeamBuilder.model._
import org.scalajs.dom
import org.scalajs.dom._

object CardResults {

  type CardSearchResult = (Card, List[Awakening])
  type CardRanker = CardSearchResult => Long

  val ID_CUTOFF = 20000
  val idRanker: CardSearchResult => Long = _._1.id

  def getAtkMult(
      card: Card,
      supers: List[Awakening],
      awakeningsToConsider: Seq[Awakening]
  ): Long = {
    (card.awakenings
      .filter(awakeningsToConsider.contains)
      .map(_.atkMult)
      .fold(1.0)(_ * _) * supers
      .filter(awakeningsToConsider.contains)
      .map(_.atkMult)
      .fold(1.0)(Math.max) * card.cardStats.finalAtk).round
  }
  val atkRanker: CardSearchResult => Long = csr =>
    getAtkMult(csr._1, csr._2, Awakening.values)

  def getRcvMult(
      card: Card,
      supers: List[Awakening],
      awakeningsToConsider: Seq[Awakening]
  ): Long = {
    (card.awakenings
      .filter(awakeningsToConsider.contains)
      .map(_.rcvMult)
      .fold(1.0)(_ * _) * supers
      .filter(awakeningsToConsider.contains)
      .map(_.rcvMult)
      .fold(1.0)(Math.max) * card.cardStats.finalRcv).round
  }

  val rcvRanker: CardSearchResult => Long = csr =>
    getRcvMult(csr._1, csr._2, Awakening.values)

  val resultsRanker: Var[CardRanker] = Var(atkRanker)

  val isDesc: Var[Boolean] = Var(true)

  def renderCardResults(
      results: Signal[Vector[CardSearchResult]]
  ): HtmlElement = {
    div(
      renderSortOptions(resultsRanker, isDesc),
      div(
        children <-- results
          .combineWith(resultsRanker)
          .combineWith(isDesc)
          .mapN((v, ranker, isdesc) => {
            val o = v.map(c => (c, ranker(c))).sortBy(_._2)
            if (isdesc)
              o.reverse
            else o
          })
          .map(_.slice(0, 200))
          .map(v => v.map((csr, rank) => (csr._1, csr._2, rank)))
          .split(t => (t._1.id, t._2, t._3))((key, t, sig) => {
            val card = t._1
            val supers = t._2
            val rank = t._3
            div(
              span(s"#${card.id} - ${card.name} - ${rank}"),
              br(),
              card.awakenings.map(Util.renderAwk),
              br(),
              card.superAwakenings.map(awk => {
                if (!supers.contains(awk))
                  Util.renderAwk(awk).amend(className := "gray")
                else
                  Util.renderAwk(awk)
              })
            )
          })
      )
    )
  }

  val labelToRanker: Map[String, CardSearchResult => Long] = Map(
    "ATK" -> atkRanker,
    "RCV" -> rcvRanker,
    "id" -> idRanker
  )
  def renderSortOptions(
      resultsRanker: Var[CardRanker],
      isDesc: Var[Boolean]
  ): HtmlElement = {
    p(
      "Sort: ",
      select(
        value := "ATK",
        onChange.mapToValue.map(labelToRanker) --> resultsRanker,
        labelToRanker
          .map(t => option(value := t._1, t._1))
          .toList // toList is necessary becauses iterable is not accepted
      ),
      input(
        typ := "checkbox",
        name := "reversed",
        idAttr := "reversed",
        checked <-- isDesc.signal.map(!_),
        onInput.mapToChecked.map(!_) --> isDesc
      ),
      label(
        forId := "reversed",
        "Reversed"
      )
    )
  }
}
