package padTeamBuilder.util

object Util {
  // https://www.scala-js.org/doc/semantics.html
  def isIntType(c: Class[_]): Boolean = {
    c == classOf[java.lang.Byte] ||
    c == classOf[java.lang.Short] ||
    c == classOf[java.lang.Integer]
  }

}
