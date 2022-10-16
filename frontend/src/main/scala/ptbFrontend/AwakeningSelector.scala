package ptbFrontend

import com.raquo.airstream.web._
import com.raquo.laminar.api.L._
import padTeamBuilder.model._
import org.scalajs.dom
import org.scalajs.dom._

object AwakeningSelector {
  def renderAwakeningSelector(
      selectedAwakenings: Var[List[Awakening]]
  ) = {
    div(
      div(
        Awakening.values
          .filter(!_.name.startsWith("Unknown"))
          .filter(_ != Awakening.None)
          .filter(_ != Awakening.Super)
          .map(awk => {
            val ele = Util.renderAwk(awk)
            ele.amend(
              onClick.map((_) => {
                selectedAwakenings.now() :+ awk
              }) --> selectedAwakenings
            )
          }): _*
      ),
      div(
        h1("Selected: "),
        div(
          children <-- selectedAwakenings.signal
            .map(a => {
              a.zipWithIndex.map((awk, ind) => {
                Util
                  .renderAwk(awk)
                  .amend(
                    onClick.map((_) => {
                      val l = selectedAwakenings.now()
                      l.slice(0, ind) ++ l.slice(ind + 1, l.size)
                    }) --> selectedAwakenings
                  )
              })
            })
        )
      )
    )
  }
}
