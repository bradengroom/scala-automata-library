scala-automata-library
======================

Usuage
-------
###Create Automata

####Manually

    //imports
    import edu.uab.cis.State
    import edu.uab.cis.regular.Automaton

    //Create states
    val start: State = new State()
    val end: State = new State()
    
    //Create Transitions
    //Transitions are tuples: (StartState, Character, EndState)
    //Epsilon transitions are created by transitioning on '\0'
    //ex: (start, '\0', end)
    val transition: (State, Char, State) = (start, 'a', end)
    
    //Create Automaton
    //This Automaton will accept only the character a
    val automaton: Automaton = new Automaton(start, Set(end), Set(transition))
    
####Regex String

    //import
    import edu.uab.cis.regular.Automaton
    
    //Create Automaton
    //This automaton accepts one or more a's followed by a b
    val automaton: Automaton = "(a+)b"

####Regex Functions

    //imports
    import edu.uab.cis.regular.Automaton
    import edu.uab.cis.regular.Regex._
    
    //Create Automaton
    //This automaton is equal to the previous one
    val automaton: Automaton = (a+)+b
    
###Run Automata
    
    //Using the automaton in the previous example:
    automaton.accepts("ab")                //true
    automaton.accepts("aaaaaaab")          //true
    automaton.accepts("aaaa")              //false

### Automata Closure Operations

    //Concatenation
    automaton1 + automaton2 //or
    automaton1 concatenate automaton2 //or
    automaton1 concatenate List(automaton2, automaton3, automaton4) //or
    automaton1 ++ List(automaton2, automaton3, automaton4)

    //Union
    automaton1 | automaton2  //or
    automaton1 union automaton2 //or
    automaton1 union List(automaton2, automaton3, automaton4)

    //Complement
    !automaton  //or
    automaton.complement
    automaton1.relativeComplement(automaton2)       //complement relative to another automaton
    automaton1.relativeComplement(Set('a','b','c')) //complement relative to an alphabet
    
    //Intersection
    automaton1 & automaton2  //or
    automaton1 intersect automaton2
    
    //Subtraction
    automaton1 minus automaton2  //or
    automaton1 - automaton2
    
    //Reversal
    automaton.reverse

    //Repetitions
    automaton*               //or
    automaton.repeat         //0 or more times
    automaton+               //1 or more times
    automaton.optional       //or
    automaton?               //0 or 1 times
    automaton.repeat(x)      //x or more times
    automaton.repeat(x,y)    //x to y times



