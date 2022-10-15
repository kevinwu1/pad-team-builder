package ptbFrontend

import com.raquo.laminar.api.L._
import org.scalajs.dom
import org.scalajs.dom._
import padTeamBuilder.model._
import padTeamBuilder.json._
import com.raquo.airstream.web._

import play.api.libs.json._

// the name 'Laminar101' matches the 'main' method setting in the
// build.sbt file (along with the package name 'alvin').
object PadTeamBuilder {
  val cards: Var[Vector[Card]] = Var(Vector())

  val awks: Var[List[Awakening]] = Var(List())

  // the 'main' method
  def main(args: Array[String]): Unit = {

    val jsoncards = AjaxEventStream
      .get("parsed_cards.json")
      .map(req => {
        JsonParsing.cardsFromJson(req.responseText)
      })

    jsoncards --> cards
    val rootElement: HtmlElement = div(
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
        Awakening.values
          .map(renderAwk)
          .map(t => {
            val ord = t._2
            val awk = t._1
            awk.amend(
              onClick.map((_) => {
                awks.now() :+ ord
              }) --> awks
            )
          }): _*
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
