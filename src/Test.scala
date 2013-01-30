import edu.uab.cis.regular.Automaton
import edu.uab.cis.regular.Regex._
object Test {

  def main(args: Array[String]) = {
    val automaton: Automaton = "cat|dog"
      
    println(automaton accepts "dot")
  }
}