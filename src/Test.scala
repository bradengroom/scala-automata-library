import edu.uab.cis.regular.Automaton._
import edu.uab.cis.contextfree._
import edu.uab.cis.regular.Regex._

object Test {

  def main(args: Array[String]) = {

    println("(a|b)+c" accepts "ac")
  }
}