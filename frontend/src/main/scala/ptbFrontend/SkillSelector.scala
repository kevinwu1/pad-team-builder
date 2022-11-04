package ptbFrontend

import com.raquo.airstream.web._
import com.raquo.laminar.api.L._
import org.scalajs.dom
import org.scalajs.dom._
import org.scalajs.dom.raw.HTMLElement
import padTeamBuilder.model._
import padTeamBuilder.skills._
import padTeamBuilder.skills.effects.active._
import padTeamBuilder.util.SkillEffectFieldType
import padTeamBuilder.util.Util

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

  case class ASSelect(
      child: Option[ASFilter],
      selectedName: String,
      val options: Map[String, Option[ASFilter]] = Map(
        "" -> None,
        "Transform" -> Some(
          ASSingle(TransformGeneric())
        ),
        "EvolvingEffect" -> Some(
          ASSingle(EvolvingEffect(false, List()))
        ),
        "Conditional" -> Some(ASSingle(ConditionalEffect(null, null))),
        "ChangeTheWorld" ->
          Some(ASMinMax(ChangeTheWorld(0), ChangeTheWorld(100))),
        "CounterAttack" -> Some(
          ASMinMax(
            CounterAttackSkill(0, Attribute.NONE, 0),
            CounterAttackSkill(9999, Attribute.NONE, 9999)
          )
        ),
        "Suicide" -> Some(ASMinMax(Suicide(0.0), Suicide(100.0))),
        "DefenseBreak" -> Some(
          ASMinMax(
            DefenseBreak(0, 1),
            DefenseBreak(100, 10)
          )
        ),
        "Delay" -> Some(ASMinMax(Delay(0), Delay(100))),
        "EnhanceOrbs" -> Some(
          ASMinMax(EnhanceOrbs(Attribute.NONE), EnhanceOrbs(Attribute.NONE))
        ),
        "Haste" -> Some(
          ASSelect(
            Some(ASMinMax(Haste(0), Haste(100))),
            "Any",
            Map(
              "Any" -> Some(ASMinMax(Haste(0), Haste(100))),
              "HasteFixed" -> Some(ASMinMax(HasteFixed(0), HasteFixed(100))),
              "HasteRandom" -> Some(
                ASMinMax(HasteRandom(0, 0), HasteRandom(100, 100))
              )
            )
          )
        ),
        "NoSkyfall" -> Some(ASMinMax(NoSkyfallSkill(0), NoSkyfallSkill(100)))
      )
  ) extends ASFilter {
    override def test(t: SkillEffect) = child.map(_.test(t)).getOrElse(true)
    override def render(
        contextBuilder: ASFilter => ASFilter,
        asFilterState: Var[ASFilter]
    ) = {
      div(
        select(
          options.keys.toVector.sorted
            .map(k => {
              val v = options(k)
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
          onChange.mapToValue.map(v => {
            println(s"Selected $v, ${options(v)}")
            contextBuilder(
              ASSelect(
                options(v),
                v,
                options
              )
            )
          }) --> asFilterState
        ),
        child.map(
          _.render(
            x => contextBuilder(ASSelect(Option(x), selectedName, options)),
            asFilterState
          )
        )
      )
    }
  }

  def makeDefault = () => ASSelect(None, "")

  case class ASSingle[T <: SkillEffectGeneric](skillEffect: T)
      extends ASFilter {
    override def test(se: SkillEffect) = skillEffect <= se && se <= skillEffect
    override def render(
        contextBuilder: ASFilter => ASFilter,
        asFilterState: Var[ASFilter]
    ) = {
      div()
    }
  }

  case class ASAnd(exps: List[ASFilter] = List(makeDefault()))
      extends ASFilter {
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
      r.tail.foldLeft(List(r.head))((l, i) => (l :+ p("and")) :+ i) :+
        button(
          onClick.mapTo(
            myContextBuilder(
              ASAnd(exps :+ makeDefault())
            )
          ) --> asFilterState,
          "+"
        )

    })
  }
  case class ASMinMax[T <: SkillEffectGeneric](
      min: T,
      max: T
  ) extends ASFilter {
    def test(t: SkillEffect): Boolean = {
      (min <= t) && (t <= max)
    }

    def renderValueInput(
        v: Any,
        newValueHandler: String => ASFilter,
        asFilterState: Var[ASFilter]
    ): HtmlElement = {
      input(
        typ := "number",
        value := v.toString,
        inContext { thisNode =>
          onBlur
            .mapTo(thisNode.ref.value)
            .map(newValueHandler) --> asFilterState
        },
        className := s"minMax minMaxnumber"
      )
    }

    def renderMinMaxField(
        minVal: Any,
        maxVal: Any,
        seft: SkillEffectFieldType,
        myContextBuilder: ASFilter => ASFilter,
        asFilterState: Var[ASFilter],
        fieldName: String
    ): HtmlElement = {
      println(s"renderField $fieldName minval : $minVal, ${minVal.getClass}")
      val targetClass = minVal.getClass
      if (Util.isIntType(minVal.getClass())) {
        p(
          className := "minMaxItem",
          renderValueInput(
            minVal,
            newValue => {
              myContextBuilder(
                this.copy(min =
                  min.withNewField(
                    fieldName,
                    Util.convertAny(newValue, targetClass, seft)
                  )
                )
              )
            },
            asFilterState
          ),
          div(s" <= $fieldName <= "),
          renderValueInput(
            maxVal,
            newValue => {
              myContextBuilder(
                this.copy(max =
                  max.withNewField(
                    fieldName,
                    Util.convertAny(newValue, targetClass, seft)
                  )
                )
              )
            },
            asFilterState
          )
        )
      } else {
        minVal.getClass match {
          case c if classOf[Attribute].isAssignableFrom(c) => {
            p(
              select(
                (Attribute.NONE +: Attribute.fwwld).map(att => {
                  option(
                    value := att.name,
                    if (att == Attribute.NONE) "ANY" else att.name,
                    if (att == minVal)
                      Some(defaultSelected := true)
                    else
                      None
                  )
                }),
                inContext { thisNode =>
                  onChange
                    .mapTo(thisNode.ref.value)
                    .map(newValue => {
                      myContextBuilder(
                        this.copy(
                          min = min.withNewField(
                            fieldName,
                            Util.convertAny(newValue, targetClass, seft)
                          ),
                          max = max.withNewField(
                            fieldName,
                            Util.convertAny(newValue, targetClass, seft)
                          )
                        )
                      )
                    }) --> asFilterState
                }
              )
            )
          }
          case c => {
            println(s"Case not matched! $c ::: ${classOf[Attribute]}")
            ???
          }
        }
      }
    }

    override def render(
        myContextBuilder: ASFilter => ASFilter,
        asFilterState: Var[ASFilter]
    ) = {
      val names = min.productElementNames.toList
      val vals = min.productIterator.zip(max.productIterator).toList
      val sefts = min.getFieldTypes
      println(s"Render minmax fields: $min ::: $names")
      div(
        className := min.toString + ",,," + max.toString,
        div(
          className := "minMaxContainer",
          names
            .lazyZip(vals)
            .lazyZip(sefts)
            .map((fieldName, fieldValTup, seft) => {
              renderMinMaxField(
                fieldValTup._1,
                fieldValTup._2,
                seft,
                myContextBuilder,
                asFilterState,
                fieldName
              )
            })
            .toList
        )
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
