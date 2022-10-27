package padTeamBuilder.main
import upickle.default._
object PickleExample {

  def main(args: Array[String]): Unit = {
    implicit val caseBRW: ReadWriter[CaseB] = macroRW[CaseB]
    implicit val caseCRW: ReadWriter[CaseC] = macroRW[CaseC]
    implicit val traitARW: ReadWriter[TraitA] = macroRW[TraitA]
    implicit val outerRW: ReadWriter[Outer] = macroRW[Outer]
    val obj = Vector(Outer(CaseB()))
    val written = write(obj)
    println(written)
    println(read[Vector[Outer]](written))
  }
}

case class Outer(a: TraitA)

sealed trait TraitA

case class CaseB() extends TraitA
case class CaseC(i: Int) extends TraitA
