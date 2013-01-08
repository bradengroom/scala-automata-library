import edu.uab.cis.automaton._

object Main {

  def main(args: Array[String]) = {
    

    
    //println((a union Set(b,c,d)).accepts("b"))
    
    //        println(((a)).isFinite)
    //a.minimize.print
    //println(a accepts "aa")

//    val state1 = new State()
//    state1.setInitial(true)
//    val state2 = new State()
//    state2.setFinal(true)
//    val transition = new Transition(state2, 'a')
//    state1.addTransition(transition)
//    val auto = new Automaton(Set(state1, state2))
//
//    println(a accepts "")
    
    
    //    (((a)|(b))).print
    //    println("****************")
    //    (((a)|(b))).getDFA.states.foreach(state => {println(state.getId); state.associatedStates.foreach(aState => println("   " + aState.getId))})

    //    	  println(((((a+b)*)|((c+d)*))+(((e+f)*)|((g+h)*))).states.size)
    //    	  println("****************")
//        	  println(((c+d)*)+((a+b)*) accepts "ab")
//        	  println("****************")

    //    val intersectTest1 = new Automaton
    //    val intersectTest2 = new Automaton
    //
    //    val state1 = new State()
    //    	state1.setInitial(true)
    //    val state2 = new State()
    //    	state2.setFinal(true)
    //    val state3 = new State()
    //    
    //    state1.addTransition(new Transition(state2, '0'))
    //    state1.addTransition(new Transition(state3, '1'))
    //    state2.addTransition(new Transition(state2, '0'))
    //    state2.addTransition(new Transition(state2, '1'))
    //    state3.addTransition(new Transition(state3, '0'))
    //    state3.addTransition(new Transition(state3, '1'))
    //    
    //    intersectTest1.addState(state1)
    //    intersectTest1.addState(state2)
    //    intersectTest1.addState(state3)
    //    
    //    val state4 = new State()
    //    	state4.setInitial(true)
    //    val state5 = new State()
    //    	state5.setFinal(true)
    //    	
    //    state4.addTransition(new Transition(state5, '0'))
    //    state4.addTransition(new Transition(state4, '1'))
    //    state5.addTransition(new Transition(state5, '0'))
    //    state5.addTransition(new Transition(state4, '1'))
    //    
    //    intersectTest2.addState(state4)
    //    intersectTest2.addState(state5)
    //    
    //    //intersectTest1.print
    //    println("**********************")
    //    //intersectTest2.print
    //    println("**********************")
    //    (intersectTest1 intersect intersectTest2).print

    //	  println((a|b) accepts "a")
    //	  println(((a+c)*) accepts "acacacacacacb")
    //	  
    //	  a.getNextStates(Set(a.getInitialState)).foreach(_.print)

    //	  val b = new Automaton()
    //	  
    //	  val state1 = new State()
    //	  	state1.setInitial(true)
    //	  val state2 = new State()
    //	  	state2.setFinal(true)
    //	  val state3 = new State()
    //	  	state3.setFinal(true)
    //	  val state4 = new State()	  	
    //	  val state5 = new State()
    //	  val state6 = new State()
    //	    state6.setFinal(true)
    //	  val state7 = new State()
    //	    
    //	  state1.addTransition(new Transition(state3,'b'))
    //	  state1.addTransition(new Transition(state2,'a'))
    //	  
    //	  state2.addTransition(new Transition(state4,'a'))
    //	  state2.addTransition(new Transition(state5,'b'))
    //	  
    //	  state3.addTransition(new Transition(state5,'a'))
    //	  state3.addTransition(new Transition(state4,'b'))
    //	  
    //	  state4.addTransition(new Transition(state6,'a'))
    //	  state4.addTransition(new Transition(state6,'b'))
    //	  
    //	  state5.addTransition(new Transition(state6,'a'))
    //	  state5.addTransition(new Transition(state6,'b'))
    //	  
    //	  state6.addTransition(new Transition(state6,'a'))
    //	  state6.addTransition(new Transition(state6,'b'))
    //	  state6.addTransition(new Transition(state7,'c'))
    //	  
    //	  
    //	  b.addState(state1)
    //	  b.addState(state2)
    //	  b.addState(state3)
    //	  b.addState(state4)
    //	  b.addState(state5)
    //	  b.addState(state6)
    //	  b.addState(state7)
    //	  
    //	  b.print
    //	  println("**********************")
    //	  b.removeDeadStates.print
    ////	  println("**********************")
    ////	  b.getDFA.print

    //    val a = new Automaton()
    //    val state1 = new State()
    //    state1.setInitial(true)
    //    val state2 = new State()
    //    val state3 = new State()
    //    state1.addTransition(new Transition(state2, 'a'))
    //    state2.addTransition(new Transition(state3, 'b'))
    //    a.addState(state1)
    //    a.addState(state2)
    //    a.addState(state3)
    //    
    //    println(a.isEmpty)
  }

  def basicAutomaton(char: Char): Automaton = {
    val state1 = new State()
    state1.setInitial(true)
    val state2 = new State()
    state2.setFinal(true)
    val transition = new Transition(state2, char)
    state1.addTransition(transition)
    new Automaton(Set(state1, state2))
  }

  def a(): Automaton = basicAutomaton('a')
  def b(): Automaton = basicAutomaton('b')
  def c(): Automaton = basicAutomaton('c')
  def d(): Automaton = basicAutomaton('d')
  def e(): Automaton = basicAutomaton('e')
  def f(): Automaton = basicAutomaton('f')
  def g(): Automaton = basicAutomaton('g')
  def h(): Automaton = basicAutomaton('h')
  def i(): Automaton = basicAutomaton('i')
  def j(): Automaton = basicAutomaton('j')
  def k(): Automaton = basicAutomaton('k')
  def l(): Automaton = basicAutomaton('l')
  def m(): Automaton = basicAutomaton('m')
  def n(): Automaton = basicAutomaton('n')
  def o(): Automaton = basicAutomaton('o')
  def p(): Automaton = basicAutomaton('p')
  def q(): Automaton = basicAutomaton('q')
  def r(): Automaton = basicAutomaton('r')
  def s(): Automaton = basicAutomaton('s')
  def t(): Automaton = basicAutomaton('t')
  def u(): Automaton = basicAutomaton('u')
  def v(): Automaton = basicAutomaton('v')
  def w(): Automaton = basicAutomaton('w')
  def x(): Automaton = basicAutomaton('x')
  def y(): Automaton = basicAutomaton('y')
  def z(): Automaton = basicAutomaton('z')
  def A(): Automaton = basicAutomaton('A')
  def B(): Automaton = basicAutomaton('B')
  def C(): Automaton = basicAutomaton('C')
  def D(): Automaton = basicAutomaton('D')
  def E(): Automaton = basicAutomaton('E')
  def F(): Automaton = basicAutomaton('F')
  def G(): Automaton = basicAutomaton('G')
  def H(): Automaton = basicAutomaton('H')
  def I(): Automaton = basicAutomaton('I')
  def J(): Automaton = basicAutomaton('J')
  def K(): Automaton = basicAutomaton('K')
  def L(): Automaton = basicAutomaton('L')
  def M(): Automaton = basicAutomaton('M')
  def N(): Automaton = basicAutomaton('N')
  def O(): Automaton = basicAutomaton('O')
  def P(): Automaton = basicAutomaton('P')
  def Q(): Automaton = basicAutomaton('Q')
  def R(): Automaton = basicAutomaton('R')
  def S(): Automaton = basicAutomaton('S')
  def T(): Automaton = basicAutomaton('T')
  def U(): Automaton = basicAutomaton('U')
  def V(): Automaton = basicAutomaton('V')
  def W(): Automaton = basicAutomaton('W')
  def X(): Automaton = basicAutomaton('X')
  def Y(): Automaton = basicAutomaton('Y')
  def Z(): Automaton = basicAutomaton('Z')
}