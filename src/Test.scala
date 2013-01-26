import edu.uab.cis.regular._
import edu.uab.cis.contextfree._
import edu.uab.cis.Conversions._
import edu.uab.cis.regular.Regex._

object Test {

  def main(args: Array[String]) = {

    val cfgA: CFG = a
    val cfgB: CFG = b
    println (cfgA+cfgB)
  }
}