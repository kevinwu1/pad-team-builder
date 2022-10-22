package ptbFrontend

import com.raquo.airstream.web._
import com.raquo.laminar.api.L._
import org.scalajs.dom
import org.scalajs.dom._
import org.scalajs.dom.raw.HTMLElement
import padTeamBuilder.model._
import padTeamBuilder.skills._
import padTeamBuilder.skills.effects.active._

import scala.compiletime._
import scala.deriving.*

object SkillSelector {

  sealed trait ASExpression {
    def test(t: SkillEffect): Boolean
    def render: HtmlElement
  }

  case class ASNone() extends ASExpression {
    override def test(t: SkillEffect) = true
    override def render = div()
  }

  case class ASAnd(exps: List[ASExpression]) extends ASExpression {
    override def test(t: SkillEffect): Boolean = exps.forall(_.test(t))
    override def render = div({
      val r = exps.map(e => p(e.render))
      r.tail.foldLeft(List(r.head))((l, i) => (l :+ p("and")) :+ i)
    })
  }
  case class ASMinMax[T <: SkillEffect](min: T, max: T) extends ASExpression {
    def test(t: SkillEffect): Boolean = {
      (min <= t) && (t <= max)
    }

    override def render = {
      val names = min.productElementNames.toList
      val vals = min.productIterator.zip(max.productIterator).toList
      div(
        div(min.getClass().getSimpleName()),
        names.zip(vals).map((n, t) => div(s"${t._1} <= $n <= ${t._2}"))
      )
    }
  }

  inline def getSomeVals[T <: SkillEffect](using
      x: Mirror.ProductOf[T]
  ): String = {
    val s = helper[x.MirroredElemTypes]
    s.mkString(",")
  }

  inline def helper[T <: Tuple]: List[Int] = inline erasedValue[T] match {
    case _: EmptyTuple  => Nil
    case _: (Int *: ts) => 0 :: helper[ts]
    case _: (t *: ts)   => -1 :: helper[ts]
  }

  def renderSkillSelect(using
      x: Mirror.ProductOf[DefenseBreak]
  ): HtmlElement = {
    /**/
    ???
  }

  def renderSkillSelector(
      asExpression: Var[ASExpression]
  ) = {
    div(
      asExpression.now().render
    )
    // div(
    //   div(
    //     Awakening.values
    //       .filter(!_.name.startsWith("Unknown"))
    //       .filter(_ != Awakening.None)
    //       .filter(_ != Awakening.Super)
    //       .map(awk => {
    //         val ele = Util.renderAwk(awk)
    //         ele.amend(
    //           onClick.map((_) => {
    //             selectedAwakenings.now() :+ awk
    //           }) --> selectedAwakenings
    //         )
    //       }): _*
    //   ),
    //   div(
    //     h1("Selected: "),
    //     div(
    //       children <-- selectedAwakenings.signal
    //         .map(a => {
    //           a.zipWithIndex.map((awk, ind) => {
    //             Util
    //               .renderAwk(awk)
    //               .amend(
    //                 onClick.map((_) => {
    //                   val l = selectedAwakenings.now()
    //                   l.slice(0, ind) ++ l.slice(ind + 1, l.size)
    //                 }) --> selectedAwakenings
    //               )
    //           })
    //         })
    //     )
    //   )
    // )
  }

}
