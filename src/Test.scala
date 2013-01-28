import edu.uab.cis.regular.Automaton._
import edu.uab.cis.regular._
import edu.uab.cis._
import edu.uab.cis.contextfree._
import edu.uab.cis.regular.Regex._

object Test {

  def main(args: Array[String]) = {
    val auto = "(a+)+(b+)".removeNondistinguishableStates
    println(auto)
  }
}