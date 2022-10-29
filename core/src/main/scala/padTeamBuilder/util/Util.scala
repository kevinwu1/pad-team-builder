package padTeamBuilder.util

import scala.compiletime._
import scala.deriving.*
import padTeamBuilder.skills.ActiveSkill
import padTeamBuilder.model.{Attribute, Awakening}
import padTeamBuilder.skills.effects.active.SkillEffect
import padTeamBuilder.skills.effects.active.ConditionalComponent

object Util {
  // https://www.scala-js.org/doc/semantics.html
  def isIntType(c: Class[_]): Boolean = {
    c == classOf[java.lang.Byte] ||
    c == classOf[java.lang.Short] ||
    c == classOf[java.lang.Integer]
  }

  inline def getFieldTypes[T](using
      x: Mirror.ProductOf[T]
  ): List[SkillEffectFieldType] = {
    helper[x.MirroredElemTypes]
  }
  case class Test(a: Int, b: Double, c: Boolean, d: List[ActiveSkill])

  inline def helper[T <: Tuple]: List[SkillEffectFieldType] =
    inline erasedValue[T] match {
      case _: EmptyTuple     => Nil
      case _: (Int *: ts)    => SkillEffectFieldType.INT :: helper[ts]
      case _: (Double *: ts) => SkillEffectFieldType.DOUBLE :: helper[ts]
      case _: (_ *: ts)      => SkillEffectFieldType.OTHER :: helper[ts]
    }

  def convertAny(
      newValue: String,
      targetClass: Class[_],
      seft: SkillEffectFieldType
  ): Any = {
    seft match {
      case SkillEffectFieldType.DOUBLE => newValue.toDouble
      case SkillEffectFieldType.INT    => newValue.toDouble.floor.toInt
      case SkillEffectFieldType.OTHER => {
        targetClass match {
          case c if classOf[Attribute].isAssignableFrom(targetClass) =>
            Attribute.valueOf(newValue)
        }
      }
    }
  }
}
