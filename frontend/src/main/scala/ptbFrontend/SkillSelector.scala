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

  sealed trait ASFilter {
    def test(t: SkillEffect): Boolean
    def render(
        contextBuilder: ASFilter => ASFilter,
        asFilterState: Var[ASFilter]
    ): HtmlElement
  }

  case class ASNone() extends ASFilter {
    override def test(t: SkillEffect) = true
    override def render(
        contextBuilder: ASFilter => ASFilter,
        asFilterState: Var[ASFilter]
    ) = div(
      select(
        option(
          value := "defensebreak",
          "defense break"
        )
      )
    )
  }

  case class ASAnd(exps: List[ASFilter]) extends ASFilter {
    override def test(t: SkillEffect): Boolean = exps.forall(_.test(t))
    override def render(
        myContextBuilder: ASFilter => ASFilter,
        asFilterState: Var[ASFilter]
    ) = div({
      val r = exps.zipWithIndex.map((e, index) =>
        p(
          e.render(
            contextBuilder = (newFilter: ASFilter) => {
              myContextBuilder(
                ASAnd(
                  exps.patch(index, List(newFilter), 1)
                )
              )
            },
            asFilterState
          )
        )
      )
      r.tail.foldLeft(List(r.head))((l, i) => (l :+ p("and")) :+ i)
    })
  }
  case class ASMinMax[T <: SkillEffect](
      min: T,
      max: T,
      mapFun: Map[String, Any] => T
  ) extends ASFilter {
    def test(t: SkillEffect): Boolean = {
      (min <= t) && (t <= max)
    }

    def convert[T](s: String, targetClass: Class[T]): T = {
      val sReal = if (s.isEmpty()) "999999" else s
      (targetClass match {
        case c if c == classOf[Long] => sReal.toLong
      }).asInstanceOf[T]
    }

    override def render(
        myContextBuilder: ASFilter => ASFilter,
        asFilterState: Var[ASFilter]
    ) = {
      val names = min.productElementNames.toList
      val vals = min.productIterator.zip(max.productIterator).toList
      div(
        div(min.getClass().getSimpleName()),
        names
          .zip(vals)
          .map((fieldName, fieldValTup) => {
            div(
              input(
                typ := "number",
                inContext { thisNode =>
                  onBlur
                    .mapTo(thisNode.ref.value)
                    .map(v => {
                      val newFieldsMap = min.productElementNames
                        .zip(min.productIterator)
                        .map((fname, fval) => {
                          fname -> (
                            if (fname == fieldName)
                              convert(v, fval.getClass)
                            else fval
                          )
                        })
                        .toMap

                      myContextBuilder(
                        this.copy(min = mapFun(newFieldsMap))
                      )
                    }) --> asFilterState
                }
              ),
              div(s" <= $fieldName <= ")
            )
          })
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
      asFilterState: Var[ASFilter]
  ) = {
    div(
      asFilterState
        .now()
        .render(x => x, asFilterState)
    )
  }

}
