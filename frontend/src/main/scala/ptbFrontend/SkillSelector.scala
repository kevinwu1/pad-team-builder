package ptbFrontend

import com.raquo.airstream.web._
import com.raquo.laminar.api.L._
import padTeamBuilder.model._
import org.scalajs.dom
import org.scalajs.dom._
import padTeamBuilder.skills.effects.active._
import padTeamBuilder.skills._

import scala.deriving.*
import scala.compiletime._

object SkillSelector {

  sealed trait ASExpression {
    def test(t: SkillEffect): Boolean
  }

  case class ASAnd(exps: List[ASExpression]) extends ASExpression {
    override def test(t: SkillEffect): Boolean = exps.forall(_.test(t))
  }
  case class ASMinMax[T <: SkillEffect](min: T, max: T) extends ASExpression {
    def test(t: SkillEffect): Boolean = {
      min.getClass().toString() == t.getClass().toString() && (
        (min <= t) && (t <= max)
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
      selectedSkill: Var[List[Awakening]]
  ) = {
    val a = ActiveSkill("ni", NoEffect(), 1, 2)

    a.productElementNames

    ???
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
