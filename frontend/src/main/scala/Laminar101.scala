import com.raquo.laminar.api.L._
import org.scalajs.dom

// the name 'Laminar101' matches the 'main' method setting in the
// build.sbt file (along with the package name 'alvin').
object Laminar101 {
  val colorVar = Var("green")
  // the 'main' method
  def main(args: Array[String]): Unit = {
    // val colorVar
    // create a <div> that contains an <h1> tag. these methods come from
    // the 'com.raquo.laminar.api.L._' import.

    val rootElement: HtmlElement = div(
      h1("Hello, world 123"),
      color <-- colorVar
    )

    println("ASDFASDF")
    // `#root` here must match the `id` in index.html
    val containerNode = dom.document.querySelector("#root")

    // this is how you render the rootElement in the browser
    render(containerNode, rootElement)
  }

}
