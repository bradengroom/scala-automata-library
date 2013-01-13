package edu.uab.cis.automaton;

class Automaton(val startState: State, val finalStates: Set[State], val transitions: Set[((State, Char), State)]) {

  //set of states and characters for this automaton
  val states = getReachableStates(this.startState)
  val alphabet = this.transitions.map(_._1._2).toSet - '\0'

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
  def complement(): Automaton = {
    //the automaton must be a DFA in order to get the complement properly
    if (this.isDeterministic)
      new Automaton(this.startState, (this.states -- this.finalStates), this.transitions)
    else
      this.getDFA.complement
  }

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
    new Automaton(newStartState, this.finalStates ++ automaton.finalStates, this.transitions ++ automaton.transitions + ((newStartState, '\0') -> this.startState) + ((newStartState, '\0') -> automaton.startState))
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
    new Automaton(newStartState, this.finalStates + newStartState, this.transitions ++ (this.finalStates.map((_, '\0') -> this.startState)) + ((newStartState, '\0') -> this.startState))
  }

  /**
   * @return Returns an automaton that accepts zero or more repetitions of the languages of the given automata
   */
  def *() = repeat()

  /**
   * @param automaton
   * @return Returns an automaton that accepts the intersection of the languages of the given automata
   */
  def intersect(automaton: Automaton): Automaton = {
    val fullAlphabet = (this.alphabet union automaton.alphabet)
    //De Morgan's Law
    ((this.getRelativeDFA(fullAlphabet).complement) | (automaton.getRelativeDFA(fullAlphabet).complement)).complement
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
    new Automaton(this.startState, automatonClone.finalStates, this.transitions ++ automatonClone.transitions ++ (this.finalStates.map((_, '\0') -> automatonClone.startState)))
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
  def minus(automaton: Automaton): Automaton = this.intersect(automaton.complement)

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
  def transition(state: State, letter: Char): Set[State] = this.transitions.filter(transition => transition._1._1 == state && transition._1._2 == letter).map(_._2)

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
    this.transitions.forall(transition => !(transition._1._2 == '\0')) &&
      //every state has the same alphabet as the initial state
      this.states.forall(state => this.transitions.filter(_._1._1 == state).map(_._1._2).toSet == this.alphabet) &&
      //each state has no more than one transition per character
      this.states.forall(state => this.alphabet.forall(letter => this.transitions.filter(transition => transition._1._1 == state && transition._1._2 == letter).size == 1))
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
      if (automaton.states.forall(state => automaton.transitions.filter(_._1._1 == state).size > 0)) {
        automaton
      } else {
        //we need to get transitions for states that do not have transitions yet
        val statesToExplore: Set[State] = automaton.states.filter(state => automaton.transitions.filter(transition => transition._1._1 == state).size == 0)
        val transitionsToCheck: Set[((State, Char), Set[State])] = statesToExplore.flatMap(state => {
          alphabet.map(letter => {
            ((state, letter), this.transitionAndEpsilonJump(state.associatedStates, letter))
          })
        })
        val statesToAdd: Set[State] = transitionsToCheck.filter(transition => {
          !automaton.states.map(_.associatedStates).contains(transition._2)
        }).map(transition => new State(transition._2))
        val transitionsToAdd: Set[((State, Char), State)] = transitionsToCheck.map(transition => {
          ((transition._1._1, transition._1._2), (automaton.states ++ statesToAdd).filter(_.associatedStates == transition._2).head)
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
  def isFinite(): Boolean = this.states.forall(state => !pathExists(state, state))

  /**
   * @return Returns true if the automaton does not accept anything
   */
  def isEmpty(): Boolean = this.finalStates.size == 0

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
  def equals(automaton: Automaton): Boolean = {
    ((this intersect (automaton.relativeComplement(this))) union ((this.relativeComplement(automaton)) intersect automaton)).isEmpty
  }

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
  def substitute(oldLetter: Char, newLetter: Char): Automaton = new Automaton(this.startState, this.finalStates, this.transitions.map(transition => if (transition._1._2 == oldLetter) ((transition._1._1, newLetter) -> transition._2) else transition))

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
  def getConnectedStates(state: State): Set[State] = this.transitions.filter(_._1._1 == state).map(_._2).toSet

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
   * @see java.lang.Object#clone()
   */
  override def clone(): Automaton = {
    val newStates = this.states.map(state => new State(Set(state)))
    val newFinalStates = newStates.filter(state => this.finalStates.contains(state.associatedStates.head))
    val newTransitions = this.transitions.map(transition => ((newStates.filter(state => state.associatedStates == Set(transition._1._1)).head, transition._1._2), newStates.filter(state => state.associatedStates == Set(transition._2)).head))
    new Automaton(newStates.filter(state => state.associatedStates.head == this.startState).head, newFinalStates, newTransitions)
  }

  /**
   * Prints the automaton
   */
  def print() = {
    println("State " + this.startState.getId + (if (this.finalStates.contains(this.startState)) " [initial][final]:" else " [initial]:"))
    this.transitions.filter(_._1._1 == this.startState).foreach(transition => {
      if (transition._1._2 == '\0') {
        println("	-> " + transition._2.getId)
      } else {
        println("	" + transition._1._2 + "-> " + transition._2.getId)
      }
    })
    (this.states - this.startState).foreach(state => {
      println("State " + state.getId + (if (this.finalStates.contains(state)) " [final]:" else " :"))
      this.transitions.filter(_._1._1 == state).foreach(transition => {
        if (transition._1._2 == '\0') {
          println("	-> " + transition._2.getId)
        } else {
          println("	" + transition._1._2 + "-> " + transition._2.getId)
        }
      })
    })
  }
}