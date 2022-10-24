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

  def lookup(k: String, m: Map[String, Option[ASFilter]]) = {
    println("looking up key " + k)
    m(k)
  }

  case class ASSelect(child: Option[ASFilter], selectedName: String)
      extends ASFilter {
    override def test(t: SkillEffect) = child.map(_.test(t)).getOrElse(true)
    override def render(
        contextBuilder: ASFilter => ASFilter,
        asFilterState: Var[ASFilter]
    ) = {
      val options: Map[String, Option[ASFilter]] = Map(
        "" -> None,
        "DefenseBreak" -> Some(
          ASMinMax(
            DefenseBreak(0, 1),
            DefenseBreak(100, 10)
          )
        )
      )
      div(
        select(
          options
            .map((k, v) => {
              option(
                value := k,
                k
                // defaultSelected := if (k == selectedName) "true"
              ).amend(
                (if (selectedName == k) List(defaultSelected := true)
                 else List()): _*
              )
            })
            .toList,
          onChange.mapToValue.map(v =>
            contextBuilder(
              ASSelect(
                options(v),
                v
              )
            )
          ) --> asFilterState
        ),
        child.map(
          _.render(
            x => contextBuilder(ASSelect(Option(x), selectedName)),
            asFilterState
          )
        )
      )
    }
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
  case class ASMinMax[T <: SkillEffectGeneric](
      min: T,
      max: T
  ) extends ASFilter {
    def test(t: SkillEffect): Boolean = {
      (min <= t) && (t <= max)
    }

    // https://www.scala-js.org/doc/semantics.html
    def isIntType(c: Class[_]): Boolean = {
      c == classOf[java.lang.Byte] || c == classOf[java.lang.Integer]
    }

    def convert(s: String, targetClass: Class[_]): Any = {
      val sReal = if (s.isEmpty()) "999999" else s
      if (isIntType(targetClass))
        sReal.toInt
      else
        targetClass match {
          case c if c == classOf[Attribute] => Attribute.HEART
        }
    }

    override def render(
        myContextBuilder: ASFilter => ASFilter,
        asFilterState: Var[ASFilter]
    ) = {
      val names = min.productElementNames.toList
      val vals = min.productIterator.zip(max.productIterator).toList
      div(
        className := min.toString + ",,," + max.toString,
        div(min.getClass().getSimpleName()),
        names
          .zip(vals)
          .map((fieldName, fieldValTup) => {
            val minVal = fieldValTup._1
            val maxVal = fieldValTup._2
            val inputType = if (isIntType(minVal.getClass())) "number" else ""
            div(
              input(
                typ := inputType,
                value := minVal.toString,
                inContext { thisNode =>
                  onBlur
                    .mapTo(thisNode.ref.value)
                    .map(newValue => {
                      myContextBuilder(
                        this.copy(min =
                          min.withNewField(
                            fieldName,
                            convert(newValue, minVal.getClass())
                          )
                        )
                      )
                    }) --> asFilterState
                }
              ),
              div(s" <= $fieldName (${minVal.getClass()}) <= "),
              input(
                typ := inputType,
                value := maxVal.toString,
                inContext { thisNode =>
                  onBlur
                    .mapTo(thisNode.ref.value)
                    .map(newValue => {
                      myContextBuilder(
                        this.copy(max =
                          max.withNewField(
                            fieldName,
                            convert(newValue, maxVal.getClass())
                          )
                        )
                      )
                    }) --> asFilterState
                }
              )
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

  // def renderSkillSelect(using
  //     x: Mirror.ProductOf[DefenseBreak]
  // ): HtmlElement = {
  //   /**/
  //   ???
  // }

  def renderSkillSelector(
      asFilterState: Var[ASFilter]
  ) = {
    div(
      child <-- asFilterState.signal.map(_.render(x => x, asFilterState))
    )
  }

}
