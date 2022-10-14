package padTeamBuilder.main

import java.io.File;
object PadTeamBuilder {
  def main(args: Array[String]) = {
    println("ASDFSDF")
    val classpath = System.getProperty("java.class.path")
    classpath.split(File.pathSeparator).foreach(println)
  }
}
