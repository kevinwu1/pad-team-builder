package ptbFrontend

import org.scalajs.dom
import org.scalajs.dom._
import padTeamBuilder.json._
import padTeamBuilder.model._
import com.raquo.laminar.api.L._
import com.raquo.airstream.web._

object Util {
  def renderAwk(awk: Awakening): HtmlElement = {
    val DIM = 36
    val AWK_PER_ROW = 11
    val basex = 0
    val basey = 324
    val col = awk.ordinal % AWK_PER_ROW
    val row = awk.ordinal / AWK_PER_ROW
    val x = basex + DIM * col
    val y = basey + DIM * row
    div(
      styleAttr :=
        s"display: inline-block; width: 36px; height: 36px; background-image: url(\"eggs.png\"); background-repeat: no-repeat; background-position: -${x}px -${y}px;"
    )
  }

  def loadCards(cards: Var[Vector[Card]]) = {
    def getCards(url: String, cards: Var[Vector[Card]], cb: () => Unit) = {
      val s = AjaxEventStream
        .get(
          url = url,
          progressObserver =
            Observer[(XMLHttpRequest, ProgressEvent)] { (evs, ev) =>
              val perc = ev.loaded * 100.0 / ev.total
            },
          readyStateChangeObserver = Observer[XMLHttpRequest] { x =>
            if (x.readyState == 4) {
              cb()
            }
          }
        )
        .map(req => {
          val p = JsonParsing.cardsFromJson(req.responseText)
          p
        })
      s.addObserver(Observer[Vector[Card]] { newCards =>
        cards.update(v => v ++ newCards)
      })(unsafeWindowOwner)
    }
    (0 to 9).foldLeft(() => ())((cb, i) =>
      () => getCards(s"parsed_cards_${i}.json", cards, cb)
    )()
  }
}
