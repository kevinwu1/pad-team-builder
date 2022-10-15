package ptbFrontend

import com.raquo.laminar.api.L._
import org.scalajs.dom
import org.scalajs.dom._
import padTeamBuilder.model._
import padTeamBuilder.skills._
import padTeamBuilder.skills.effects.active._
import padTeamBuilder.skills.effects.leader._
import padTeamBuilder.json._
import com.raquo.airstream.web._

import play.api.libs.json._
import com.raquo.airstream.core.Observer

// the name 'Laminar101' matches the 'main' method setting in the
// build.sbt file (along with the package name 'alvin').
object PadTeamBuilder {

  val cards: Var[Vector[Card]] = Var(Vector[Card]())

  def getCards(url: String, cards: Var[Vector[Card]], cb: () => Unit) = {
    println(s"getCards called on $url")
    val s = AjaxEventStream
      .get(
        url = url,
        progressObserver =
          Observer[(XMLHttpRequest, ProgressEvent)] { (evs, ev) =>
            val perc = ev.loaded * 100.0 / ev.total
            println(
              s"Progress: ${perc}% of ${ev.total}"
            )
          },
        readyStateChangeObserver = Observer[XMLHttpRequest] { x =>
          println(s"ready state: ${x.readyState}")
          if (x.readyState == 4) {
            cb()
          }
        }
      )
      .map(req => {
        println(s"Parsing start $url")
        val p = JsonParsing.cardsFromJson(req.responseText)
        println(s"Parsing end $url ::: ${p.head.id}")
        p
      })
    s.addObserver(Observer[Vector[Card]] { c =>
      println("THE SIZE IS " + c.size + ", " + c.head.id)
    })(unsafeWindowOwner)
    s.addObserver(Observer[Vector[Card]] { newCards =>
      cards.update(v => v ++ newCards)
    })(unsafeWindowOwner)
  }
  (0 to 9).foldLeft(() => ())((cb, i) =>
    () => getCards(s"parsed_cards_${i}.json", cards, cb)
  )()
  cards.signal.addObserver(Observer[Vector[Card]] { c =>
    println("THE SIZE IS " + c.size)
  })(unsafeWindowOwner)
  val awks: Var[List[Awakening]] = Var(List())

  val filteredCards: Signal[Vector[(Card, List[Awakening])]] = awks.signal
    .combineWith(cards.signal)
    .mapN((awaks, cards) => {
      println("Starting filter " + cards.size)
      val r = cards
        .map(card => {
          val (has, supers) = card.containsAwakenings(awaks, true)
          (card, has, supers)
        })
        .filter(_._2)
        .map(t => (t._1, t._3))
      println("done filter: " + r.size)
      r.slice(0, 200)
    })

  // the 'main' method
  def main(args: Array[String]): Unit = {

    val rootElement: HtmlElement = div(
      div(
        Awakening.values
          .map(renderAwk)
          .map(t => {
            val ord = t._2
            val awk = t._1
            awk.amend(
              onClick.map((_) => {
                println(cards.now().size)
                awks.now() :+ ord
              }) --> awks
            )
          }): _*
      ),
      div(
        h1("Selected: "),
        div(
          children <-- awks.signal
            .map(a => {
              a.zipWithIndex.map((awk, ind) => {
                renderAwk(awk)._1
                  .amend(
                    onClick.map((_) => {
                      val l = awks.now()
                      l.slice(0, ind) ++ l.slice(ind + 1, l.size)
                    }) --> awks
                  )
              })
            })
        )
      ),
      div(
        div("hii34"),
        div(
          children <-- filteredCards.split(t => (t._1.id, t._2))(
            (key, t, sig) => {
              val card = t._1
              val supers = t._2
              div(
                span(s"#${card.id} - ${card.name}"),
                br(),
                card.awakenings.map(renderAwk).map(_._1),
                br(),
                card.superAwakenings.map(awk => {
                  if (!supers.contains(awk))
                    renderAwk(awk)._1.amend(className := "gray")
                  else
                    renderAwk(awk)._1
                })
              )
            }
          )
        )
      )
    )

    println("ASDFASDF")
    // `#root` here must match the `id` in index.html
    val containerNode = dom.document.querySelector("#root")

    // this is how you render the rootElement in the browser
    render(containerNode, rootElement)
  }
  private def renderAwk(awk: Awakening): (HtmlElement, Awakening) = {
    val DIM = 36
    val AWK_PER_ROW = 11
    val basex = 0
    val basey = 324
    val col = awk.ordinal % AWK_PER_ROW
    val row = awk.ordinal / AWK_PER_ROW
    val x = basex + DIM * col
    val y = basey + DIM * row
    (
      div(
        styleAttr :=
          s"display: inline-block; width: 36px; height: 36px; background-image: url(\"eggs.png\"); background-repeat: no-repeat; background-position: -${x}px -${y}px;"
      ),
      awk
    )
  }

}
