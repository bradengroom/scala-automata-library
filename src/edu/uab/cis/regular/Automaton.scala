package edu.uab.cis.automaton;

import edu.uab.cis.conversions.Conversions
import edu.uab.cis.cfg._
import java.io._;

@serializable
class Automaton(val startState: State, val finalStates: Set[State], val transitions: Set[(State, Char, State)]) {

  //set of states and characters for this automaton
  val states = getReachableStates(this.startState)
  lazy val alphabet = this.transitions.map(_._2).toSet - '\0'
  lazy val deadStates = ((states -- finalStates) - startState).filter(state => this.finalStates.forall(finalState => !this.pathExists(state, finalState)))
  lazy val liveStates = this.states -- this.deadStates

  /**
   * @return Returns the number of states in this automaton
   */
  def getStateCount() = this.states.size

  /**
   * @return Returns the number of transitions in this automaton
   */
  def getTransitionCount() = this.transitions.size

  /**
   * @return Returns an automaton that accepts the complement of the language of the given automaton
   */
  def unary_!(): Automaton = {
    //the automaton must be a DFA in order to get the complement properly
    if (this.isDeterministic)
      new Automaton(this.startState, (this.states -- this.finalStates), this.transitions)
    else
      this.getDFA.unary_!
  }

  /**
   * @return Returns an automaton that accepts the complement of the language of the given automaton
   */
  def complement = this.unary_!

  /**
   * @param alphabet
   * @return Returns an automaton that represents the complement of this automaton relative to an alphabet
   */
  def relativeComplement(alphabet: Set[Char]): Automaton = this.getRelativeDFA(alphabet).complement

  /**
   * @param automaton
   * @return Returns an automaton that represents the complement of this automaton relative to another automaton
   */
  def relativeComplement(automaton: Automaton): Automaton = this.relativeComplement(this.alphabet | automaton.alphabet).intersect(automaton)

  /**
   * @param automaton
   * @return Returns an automaton that accepts the union of the languages of the given automata
   */
  def union(automaton: Automaton): Automaton = {
    val newStartState = new State()
    new Automaton(newStartState, this.finalStates ++ automaton.finalStates, this.transitions ++ automaton.transitions + ((newStartState, '\0', this.startState)) + ((newStartState, '\0', automaton.startState)))
  }

  /**
   * @param automaton
   * @return Returns an automaton that accepts the union of the languages of the given automata
   */
  def |(automaton: Automaton) = union(automaton)

  /**
   * @param automata
   * @return Returns an automaton that accepts the union of the languages of the given automata
   */
  def union(automata: Set[Automaton]): Automaton = (automata).reduceRight(_ | _) | this

  /**
   * @return Returns an automaton that accepts zero or more repetitions of the languages of the given automata
   */
  def repeat(): Automaton = {
    val newStartState = new State()
    new Automaton(newStartState, this.finalStates + newStartState, this.transitions ++ (this.finalStates.map(finalState => (finalState, '\0', this.startState))) + ((newStartState, '\0', this.startState)))
  }

  /**
   * @return Returns an automaton that accepts zero or more repetitions of the languages of the given automata
   */
  def *() = repeat()

  /**
   * @return Returns an automaton that accepts the reversal of the automatons language
   */
  def reverse(): Automaton = {
    val newStart = new State()
    val newTransitions = this.transitions.map(transition => (transition._3, transition._2, transition._1)) ++ this.finalStates.map(state => (newStart, '\0', state))
    new Automaton(newStart, Set(this.startState), newTransitions)
  }

  /**
   * @param automaton
   * @return Returns an automaton that accepts the intersection of the languages of the given automata
   */
  def intersect(automaton: Automaton): Automaton = {
    def intersect_r(intersection: Automaton): Automaton = {
      //if we have all need transitions
      if (intersection.states.forall(state => intersection.transitions.filter(_._1 == state).size > 0)) {
        intersection
      } else {
        //we need to get transitions for states that do not have transitions yet
        val statesToExplore: Set[State] = intersection.states.filter(state => intersection.transitions.filter(transition => transition._1 == state).size == 0)
        val transitionsToCheck: Set[(State, Char, Set[State])] = statesToExplore.flatMap(state => {
          (this.alphabet | automaton.alphabet).map(letter => {
            (state, letter, this.transitionAndEpsilonJump(state.associatedStates, letter) | automaton.transitionAndEpsilonJump(state.associatedStates, letter))
          })
        })
        val statesToAdd: Set[State] = transitionsToCheck.filter(transition => {
          !intersection.states.map(_.associatedStates).contains(transition._3)
        }).map(transition => new State(transition._3))
        val transitionsToAdd: Set[(State, Char, State)] = transitionsToCheck.map(transition => {
          (transition._1, transition._2, (intersection.states ++ statesToAdd).filter(_.associatedStates == transition._3).head)
        })
        intersect_r(new Automaton(intersection.startState, (intersection.states ++ statesToAdd).filter(_.associatedStates.intersect(this.finalStates).size > 0) intersect (intersection.states ++ statesToAdd).filter(_.associatedStates.intersect(automaton.finalStates).size > 0), intersection.transitions ++ transitionsToAdd))
      }
    }

    //our new start state will be a state associated with our current start states and states reachable by epsilon jump
    val newStart = new State(this.epsilonJump(this.startState) | automaton.epsilonJump(automaton.startState))
    intersect_r(new Automaton(newStart, if (this.finalStates.intersect(newStart.associatedStates).size > 0 && automaton.finalStates.intersect(newStart.associatedStates).size > 0) Set(newStart) else Set(), Set()))
  }

  /**
   * @param automaton
   * @return Returns an automaton that accepts the intersection of the languages of the given automata
   */
  def &(automaton: Automaton) = this.intersect(automaton)

  /**
   * @param automaton
   * @return Returns an automaton that accepts the concatenation of the languages of the given automata
   */
  def concatenate(automaton: Automaton): Automaton = {
    //we must use a clone of the automaton given to prevent incorrect results if it references this automaton
    val automatonClone = automaton.clone
    new Automaton(this.startState, automatonClone.finalStates, this.transitions ++ automatonClone.transitions ++ (this.finalStates.map(finalState => (finalState, '\0', automatonClone.startState))))
  }

  /**
   * @param automaton
   * @return Returns an automaton that accepts the concatenation of the languages of the given automata
   */
  def +(automaton: Automaton) = concatenate(automaton)

  /**
   * @param automata
   * @return Returns an automaton that accepts the concatenation of the languages of the given automata
   */
  def concatenate(automata: List[Automaton]): Automaton = this + (automata.reduceRight(_ + _))

  /**
   * @param automata
   * @return Returns an automaton that accepts the concatenation of the languages of the given automata
   */
  def ++(automata: List[Automaton]): Automaton = this.concatenate(automata)

  /**
   * @param automaton
   * @return Returns an automaton that accepts the subtraction of the languages of the given automata
   */
  def minus(automaton: Automaton): Automaton = {
    def minus_r(intersection: Automaton): Automaton = {
      //if we have all need transitions
      if (intersection.states.forall(state => intersection.transitions.filter(_._1 == state).size > 0)) {
        intersection
      } else {
        //we need to get transitions for states that do not have transitions yet
        val statesToExplore: Set[State] = intersection.states.filter(state => intersection.transitions.filter(transition => transition._1 == state).size == 0)
        val transitionsToCheck: Set[(State, Char, Set[State])] = statesToExplore.flatMap(state => {
          (this.alphabet | automaton.alphabet).map(letter => {
            (state, letter, this.transitionAndEpsilonJump(state.associatedStates, letter) | automaton.transitionAndEpsilonJump(state.associatedStates, letter))
          })
        })
        val statesToAdd: Set[State] = transitionsToCheck.filter(transition => {
          !intersection.states.map(_.associatedStates).contains(transition._3)
        }).map(transition => new State(transition._3))
        val transitionsToAdd: Set[(State, Char, State)] = transitionsToCheck.map(transition => {
          (transition._1, transition._2, (intersection.states ++ statesToAdd).filter(_.associatedStates == transition._3).head)
        })
        minus_r(new Automaton(intersection.startState, (intersection.states ++ statesToAdd).filter(_.associatedStates.intersect(this.finalStates).size > 0) -- (intersection.states ++ statesToAdd).filter(_.associatedStates.intersect(automaton.finalStates).size > 0), intersection.transitions ++ transitionsToAdd))
      }
    }

    //our new start state will be a state associated with our current start states and states reachable by epsilon jump
    val newStart = new State(this.epsilonJump(this.startState) | automaton.epsilonJump(automaton.startState))
    minus_r(new Automaton(newStart, if (this.finalStates.intersect(newStart.associatedStates).size > 0 && automaton.finalStates.intersect(newStart.associatedStates).size == 0) Set(newStart) else Set(), Set()))
  }

  /**
   * @param automaton
   * @return Returns an automaton that accepts the subtraction of the languages of the given automata
   */
  def -(automaton: Automaton): Automaton = this.minus(automaton)

  /**
   * @return Returns an automaton that accepts the language of the given automaton and the empty string
   */
  def optional(): Automaton = new Automaton(this.startState, this.finalStates + this.startState, this.transitions)

  /**
   * @return Returns an automaton that accepts the language of the given automaton and the empty string
   */
  def ?() = this.optional()

  /**
   * @param state
   * @param letter
   * @return Returns the resulting set of states after transitioning on the given letter from the given state
   */
  def transition(state: State, letter: Char): Set[State] = this.transitions.filter(transition => transition._1 == state && transition._2 == letter).map(_._3)

  /**
   * @param states
   * @param char
   * @return Returns the resulting set of states after transitioning on the given letter from the given set of states
   */
  def transition(states: Set[State], char: Char): Set[State] = states.flatMap(this.transition(_, char))

  /**
   * @param state
   * @return Returns the resulting set of states after making all possible epsilon jumps from the given state
   */
  def epsilonJump(state: State): Set[State] = this.epsilonJump(Set(state))

  /**
   * @param states
   * @return Returns the resulting set of states after making all possible epsilon jumps from the given set of states
   */
  def epsilonJump(states: Set[State]): Set[State] = {
    val nextStates = this.transition(states, '\0')
    if ((states union nextStates) == states)
      states
    else
      this.epsilonJump(states union nextStates)
  }

  /**
   * @param state
   * @param letter
   * @return Returns the resulting set of states after making all possible epsilon jumps from the given state, then making all possible transitions on the given letter, then making all possible epsilon jumps again
   */
  def transitionAndEpsilonJump(state: State, letter: Char): Set[State] = this.epsilonJump(this.transition(this.epsilonJump(state), letter))

  /**
   * @param states
   * @param letter
   * @return Returns the resulting set of states after making all possible epsilon jumps from the given set of states, then making all possible transitions on the given letter, then making all possible epsilon jumps again
   */
  def transitionAndEpsilonJump(states: Set[State], letter: Char): Set[State] = states.flatMap(this.transitionAndEpsilonJump(_, letter))

  /**
   * @param string
   * @return Returns true if the automaton accepts the given string
   */
  def accepts(string: String): Boolean = accepts(string.toList)

  /**
   * @param chars
   * @return Returns true if the automaton accepts the given list of characters
   */
  def accepts(chars: List[Char]): Boolean = {
    def accepts_r(chars: List[Char], currentStates: Set[State]): Boolean = {
      if (chars.size == 1)
        this.transitionAndEpsilonJump(currentStates, chars.head).intersect(this.finalStates).size > 0
      else
        accepts_r(chars.tail, this.transitionAndEpsilonJump(currentStates, chars.head))
    }

    //if we are checking if the empty string is accepted, we just need to see if the start state can reach a final state through epsilon jumps
    if (chars.size == 0)
      this.epsilonJump(this.startState).intersect(this.finalStates).size > 0
    else
      accepts_r(chars, Set(this.startState))
  }

  /**
   * @return Returns true if the automaton is deterministic
   */
  def isDeterministic(): Boolean = {
    //no state has epsilon jumps
    this.transitions.forall(transition => !(transition._2 == '\0')) &&
      //every state has the same alphabet as the initial state
      this.states.forall(state => this.transitions.filter(_._1 == state).map(_._2).toSet == this.alphabet) &&
      //each state has no more than one transition per character
      this.states.forall(state => this.alphabet.forall(letter => this.transitions.filter(transition => transition._1 == state && transition._2 == letter).size == 1))
  }

  /**
   * @return Returns the a DFA that represents the automaton
   */
  def getDFA(): Automaton = this.getRelativeDFA(this.alphabet)

  /**
   * @param alphabet
   * @return Returns the a DFA relative to the given alphabet that represents the automaton
   */
  def getRelativeDFA(alphabet: Set[Char]): Automaton = {
    def getRelativeDFA_r(automaton: Automaton): Automaton = {
      //if we have all need transitions
      if (automaton.states.forall(state => automaton.transitions.filter(_._1 == state).size > 0)) {
        automaton
      } else {
        //we need to get transitions for states that do not have transitions yet
        val statesToExplore: Set[State] = automaton.states.filter(state => automaton.transitions.filter(transition => transition._1 == state).size == 0)
        val transitionsToCheck: Set[(State, Char, Set[State])] = statesToExplore.flatMap(state => {
          alphabet.map(letter => {
            (state, letter, this.transitionAndEpsilonJump(state.associatedStates, letter))
          })
        })
        val statesToAdd: Set[State] = transitionsToCheck.filter(transition => {
          !automaton.states.map(_.associatedStates).contains(transition._3)
        }).map(transition => new State(transition._3))
        val transitionsToAdd: Set[(State, Char, State)] = transitionsToCheck.map(transition => {
          (transition._1, transition._2, (automaton.states ++ statesToAdd).filter(_.associatedStates == transition._3).head)
        })
        getRelativeDFA_r(new Automaton(automaton.startState, (automaton.states ++ statesToAdd).filter(_.associatedStates.intersect(this.finalStates).size > 0), automaton.transitions ++ transitionsToAdd))
      }
    }

    //our new start state will be a state associated with our current start state and states reachable by epsilon jump
    val newStart = new State(this.epsilonJump(this.startState))
    getRelativeDFA_r(new Automaton(newStart, if (this.finalStates.intersect(newStart.associatedStates).size > 0) Set(newStart) else Set(), Set()))
  }

  /**
   * @return Returns true if the language of the automaton is finite
   */
  def isFinite(): Boolean = this.getDFA.finalStates.forall(state => !pathExists(state, state))

  /**
   * @return Returns true if the automaton does not accept anything
   */
  def isEmpty(): Boolean = this.finalStates.forall(!this.pathExists(this.startState, _))

  /**
   * @return Returns true if the automaton accepts all strings
   */
  def isTotal(): Boolean = this.complement.isEmpty

  /**
   * @return Returns true if the automaton only accepts the empty string
   */
  def isEmptyString(): Boolean = this == BasicAutomaton.emptyString

  /**
   * @param automaton
   * @return Returns true if the given automata accept the same language
   */
  def equals(automaton: Automaton): Boolean = (this - automaton).isEmpty && (automaton - this).isEmpty

  /**
   * @param automaton
   * @return Returns true if the given automata accept the same language
   */
  def ==(automaton: Automaton): Boolean = this.equals(automaton)

  /**
   * @param automaton
   * @return Returns true if the language of the automaton is a subset of the language of the given automaton
   */
  def isSubsetOf(automaton: Automaton): Boolean = (this intersect automaton) == this

  /**
   * @param oldLetter
   * @param newLetter
   * @return Returns an automaton with all transitions on oldLetter replaced with transitions on newLetter
   */
  def substitute(oldLetter: Char, newLetter: Char): Automaton = new Automaton(this.startState, this.finalStates, this.transitions.map(transition => if (transition._2 == oldLetter) (transition._1, newLetter, transition._3) else transition))

  /**
   * @param oldLetter
   * @param string
   * @return Returns the automaton with transitions on oldLetter replaced with transitions on string
   */
  def substitute(oldLetter: Char, string: String): Automaton = this.substitute(oldLetter, BasicAutomaton.string(string))

  /**
   * @param oldLetter
   * @param automaton
   * @return Returns the automaton with transitions on oldLetter with transitions the automaton given
   */
  def substitute(oldLetter: Char, automaton: Automaton): Automaton = {
    val transitionsToChange = this.transitions.filter(transition => transition._2 == oldLetter)
    val newTransitions: Set[(State, Char, State)] = transitionsToChange.flatMap(transition => {
      val automatonClone = automaton.clone
      automatonClone.transitions ++ automatonClone.finalStates.map(finalState => (finalState, '\0', transition._3)) + Set(automatonClone.startState).map(start => (transition._1, '\0', automatonClone.startState)).head
    })
    new Automaton(this.startState, this.finalStates, (this.transitions -- transitionsToChange) ++ newTransitions)
  }

  /**
   * @param stateFrom
   * @param stateTo
   * @return Returns true if there is a path from stateFrom to stateTo
   */
  def pathExists(stateFrom: State, stateTo: State): Boolean = this.getReachableStates(stateFrom).contains(stateTo)

  /**
   * @param state
   * @return Returns the set of states that the given state can reach on any single transition
   */
  def getConnectedStates(state: State): Set[State] = this.transitions.filter(_._1 == state).map(_._3).toSet

  /**
   * @param states
   * @return Returns the set of states that the given set of states can reach on any single transition
   */
  def getConnectedStates(states: Set[State]): Set[State] = states.flatMap(this.getConnectedStates(_))

  /**
   * @param state
   * @return Returns the set of states that that the given state can reach on any number of transitions
   */
  def getReachableStates(state: State): Set[State] = getReachableStates(Set(state))

  /**
   * @param min			Minimum number of times to repeat this automaton
   * @param max			Maximum number of times to repeat this automaton
   * @Automaton			Returns the set of states that that the given set of states can reach on any number of transitions
   */
  def getReachableStates(states: Set[State]): Set[State] = {
    val allStates = states.union(this.getConnectedStates(states))
    if (allStates.subsetOf(states))
      states
    else
      getReachableStates(allStates)
  }

  /**
   * @param min	Minimum number of times to repeat this automaton
   * @Automaton	Returns an automaton that accepts min or greater repetitions of the language of the automaton
   */
  def repeat(min: Int): Automaton = {
    if (min < 1)
      this*
    else
      this + repeat(min - 1)
  }

  /**
   * @param min	Minimum number of times to repeat this automaton
   * @param max	Maximum number of times to repeat this automaton
   * @Automaton	Returns an automaton that accepts between min and max repetitions of the language of the automaton
   */
  def repeat(min: Int, max: Int): Automaton = {
    if (min > max)
      BasicAutomaton.empty
    else
      this.repeat(min) intersect this.repeat(max + 1).complement
  }
  
  /**
 * @return Returns the automaton as a CFG
 */
def toCFG(): CFG = Conversions.Automaton2CFG(this)

  /**
   * @see java.lang.Object#clone()
   */
  override def clone(): Automaton = {
    val newStates = this.states.map(state => new State(Set(state)))
    val newFinalStates = newStates.filter(state => this.finalStates.contains(state.associatedStates.head))
    val newTransitions = this.transitions.map(transition => (newStates.filter(state => state.associatedStates == Set(transition._1)).head, transition._2, newStates.filter(state => state.associatedStates == Set(transition._3)).head))
    new Automaton(newStates.filter(state => state.associatedStates.head == this.startState).head, newFinalStates, newTransitions)
  }

  /**
   * Prints the automaton
   */
  override def toString(): String = {
    def toString_r(state: State): String = {
      "State " + state.getId + (if (this.finalStates.contains(state)) " [final]:" else " :") + "\n" +
      this.transitions.filter(_._1 == state).map(transition => {
        if (transition._2 == '\0')
          "	ϵ-> " + transition._3.getId
        else 
          "	" + transition._2 + "-> " + transition._3.getId
      }).reduce(_+"\n"+_)
    }
    toString_r(this.startState) + "\n" +
    (this.states - this.startState).map(state => {
      toString_r(state)
    }).reduce(_+"\n"+_)
  }

  /**
   * @return Returns the automaton as a byte array
   */
  def toByteArray(): Array[Byte] = {
    val baos = new ByteArrayOutputStream()
    val oos = new ObjectOutputStream(baos)
    oos.writeObject(this)
    val byteArray = baos.toByteArray
    oos.close()
    baos.close()
    byteArray
  }

  /**
   * @param file
   */
  def save(file: File) = {
    val fos = new FileOutputStream(file)
    fos.write(this.toByteArray)
    fos.close()
  }

  /**
   * @param filePath
   */
  def save(filePath: String) = {
    val fos = new FileOutputStream(filePath)
    fos.write(this.toByteArray)
    fos.close()
  }
}

object Automaton {

  /**
   * @param filePath
   * @return Loads the automaton saved in the given file path and returns it
   */
  def load(filePath: String): Automaton = {
    val bais = new FileInputStream(filePath)
    val is = new ObjectInputStream(bais)
    val automaton = is.readObject().asInstanceOf[Automaton]
    bais.close()
    is.close()
    automaton
  }

  /**
   * @param file
   * @return Loads the automaton saved in the given file and returns it
   */
  def load(file: File): Automaton = {
    val bais = new FileInputStream(file)
    val is = new ObjectInputStream(bais)
    val automaton = is.readObject().asInstanceOf[Automaton]
    bais.close()
    is.close()
    automaton
  }
}