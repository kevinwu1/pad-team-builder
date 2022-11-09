package ptbFrontend

import com.raquo.airstream.web._
import com.raquo.laminar.api.L._
import org.scalajs.dom
import org.scalajs.dom._
import padTeamBuilder.json._
import padTeamBuilder.model._

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

  def renderCardThumbnailImage(
      id: Long,
      mainAtt: Attribute,
      subAtt: Attribute
  ): HtmlElement = {
    val digs = ((id % 10000) / 100 + 1).toString
    val pad = List.fill(3 - digs.size)("0").mkString

    val DIM = 102
    val CARDS_PER_ROW = 10
    val basex = 0
    val basey = 0
    val col = (id - 1 + CARDS_PER_ROW) % CARDS_PER_ROW
    val row = ((id - 1) / CARDS_PER_ROW) % CARDS_PER_ROW
    val x = basex + DIM * col
    val y = basey + DIM * row

    val CARD_FRAME_URL =
      "https://f000.backblazeb2.com/file/ilmina/extract/PAD_16.0.0.apk2/CARDFRAME2.PNG"
    val EMPTY_CARD_FRAME_URL =
      "https://f000.backblazeb2.com/file/ilmina/extract/PAD_16.0.0.apk2/CARDFRAMEW.PNG"
    def getAttributeParts(
        att: Attribute,
        isMain: Boolean
    ): (String, Int, Int) = {
      if (
        List(
          Attribute.FIRE,
          Attribute.WATER,
          Attribute.WOOD,
          Attribute.LIGHT,
          Attribute.DARK
        ).contains(att)
      ) {

        (
          CARD_FRAME_URL,
          att.ordinal() * DIM,
          if (isMain) 0 else 104
        )
      } else {
        (
          EMPTY_CARD_FRAME_URL,
          0,
          0
        )
      }
    }

    val (mainAttUrl, mainX, mainY) = getAttributeParts(mainAtt, true)
    val (subAttUrl, subX, subY) = getAttributeParts(subAtt, false)
    div(
      styleAttr := s"display: inline-block; width: 102px; height: 102px",
      div(
        styleAttr :=
          s"position:absolute; width: 102px; height: 102px; background-image: url(\"https://f000.backblazeb2.com/file/ilmina/extract/cards2/CARDS_${pad + digs}.PNG\"); background-repeat: no-repeat; background-position: -${x}px -${y}px;"
      ),
      div(
        styleAttr :=
          s"position:absolute; width: 102px; height: 102px; background-image: url(\"$mainAttUrl\"); background-repeat: no-repeat; background-position: -${mainX}px -${mainY}px;"
      ),
      if (subAttUrl == EMPTY_CARD_FRAME_URL)
        None
      else
        Some(
          div(
            styleAttr :=
              s"position:absolute; width: 102px; height: 102px; background-image: url(\"$subAttUrl\"); background-repeat: no-repeat; background-position: -${subX}px -${subY}px;"
          )
        )
    )
  }

  val CARD_PARTS = 9
  val cards: Var[Vector[Card]] = Var(Vector[Card]())

  def loadCards(): Signal[Vector[Card]] = {
    val s = AjaxEventStream
      .get(
        url = "parsed_cards.json",
        progressObserver =
          Observer[(XMLHttpRequest, ProgressEvent)] { (evs, ev) =>
            val perc = ev.loaded * 100.0 / ev.total
          },
      )
      .map(req => {
        println("parsing")
        val p = JsonParsing.cardsFromJson(req.responseText)
        println("parsed")
        p
      })
    s.addObserver(Observer[Vector[Card]] { newCards =>
      cards.update(v => v ++ newCards)
    })(unsafeWindowOwner)
    cards.signal
  }
}
