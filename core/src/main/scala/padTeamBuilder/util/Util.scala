package padTeamBuilder.util

import padTeamBuilder.model.Attribute
import padTeamBuilder.model.Awakening
import padTeamBuilder.model.CardSlot
import padTeamBuilder.skills.ActiveSkill
import padTeamBuilder.skills.effects.active.ConditionalComponent
import padTeamBuilder.skills.effects.active.SkillEffect

import scala.compiletime.*
import scala.deriving.*

object Util {
  // https://www.scala-js.org/doc/semantics.html
  def isIntType(c: Class[?]): Boolean = {
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
      targetClass: Class[?],
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

  extension (i: Int) {
    def toSlots: List[CardSlot] = CardSlot.fromBitFlag(i)
    def toAttList: List[Attribute] = Attribute.fromBitFlag(i)
  }
}
