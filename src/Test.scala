import edu.uab.cis.grammar._

object Test {

  def main(args: Array[String]) = {

//    val grammar = new Grammar(S, Set(
//    		S -> List(S, S, 'a', S)
//        ))
    val grammar = new Grammar(1, Set(
    		1 -> List('a', 1, 'b'),
    		1 -> List('\0')
        ))
    grammar.print
  }
}