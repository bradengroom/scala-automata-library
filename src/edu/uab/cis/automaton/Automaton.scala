package edu.uab.cis.automaton;

class Automaton(val startState: State, val finalStates: Set[State], val transitions: Set[((State, Char), State)]) {

  //  def this(startState: State, finalStates: Set[State], transitions: Map[(State, Char), State]) = this(Set(), startState, finalStates, transitions)

  val states = getReachableStates(this.startState)
  val alphabet: Set[Char] = this.transitions.map(_._1._2).toSet - '\0'

  def getStateCount(): Int = this.states.size
  def getTransitionCount(): Int = this.transitions.size

  def complement(): Automaton = if ( this.isDeterministic) new Automaton(this.startState, (this.states -- this.finalStates), this.transitions) else this.getDFA.complement

  //  def relativeComplement(alphabet: Set[Char]): Automaton = new Automaton(this.getRelativeDFA(alphabet).states.map((state: State) => state.complement))
  //def relativeComplement(automaton: Automaton): Automaton = this.complement.intersect(automaton)

  def union(automaton: Automaton): Automaton = {
    val newStartState = new State()
    new Automaton(newStartState, this.finalStates ++ automaton.finalStates, this.transitions ++ automaton.transitions + ((newStartState, '\0') -> this.startState) + ((newStartState, '\0') -> automaton.startState))
  }
  def |(automaton: Automaton) = union(automaton)
  def union(automata: Set[Automaton]): Automaton = (automata).reduceRight(_ | _) | this

  def repeat(): Automaton = {
    val newStartState = new State()
    new Automaton(newStartState, this.finalStates + newStartState, this.transitions ++ (this.finalStates.map((_, '\0') -> this.startState)) + ((newStartState, '\0') -> this.startState))
  }
  def *() = repeat()

  def intersect(automaton: Automaton): Automaton = {
    val fullAlphabet = (this.alphabet union automaton.alphabet)
    //De Morgan's Law
    ((this.getRelativeDFA(fullAlphabet).complement) | (automaton.getRelativeDFA(fullAlphabet).complement)).complement
  }

  def concatenate(automaton: Automaton): Automaton = new Automaton(this.startState, automaton.finalStates, this.transitions ++ automaton.transitions ++ (this.finalStates.map((_, '\0') -> automaton.startState)))
  def +(automaton: Automaton) = concatenate(automaton)

  def concatenate(automata: Set[Automaton]): Automaton = this + (automata).reduceRight(_ + _)
  def ++(automata: Set[Automaton]): Automaton = this.concatenate(automata)

  def minus(automaton: Automaton): Automaton = this.intersect(automaton.complement)
  def -(automaton: Automaton): Automaton = this.minus(automaton)

  def optional(): Automaton = new Automaton(this.startState, this.finalStates + this.startState, this.transitions)
  def ?() = this.optional()

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

  def transition(state: State, char: Char): Set[State] = this.transitions.filter(transition => transition._1._1 == state && transition._1._2 == char).map(_._2)
  def transition(states: Set[State], char: Char): Set[State] = states.flatMap(this.transition(_, char))

  def epsilonJump(state: State): Set[State] = this.transition(state, '\0') + state
  def epsilonJump(states: Set[State]): Set[State] = this.transition(states, '\0') ++ states

  def transitionAndEpsilonJump(state: State, char: Char): Set[State] = this.epsilonJump(this.transition(this.epsilonJump(state), char))
  def transitionAndEpsilonJump(states: Set[State], char: Char): Set[State] = states.flatMap(this.transitionAndEpsilonJump(_, char))

  def accepts(string: String): Boolean = accepts(string.toList)

  def accepts(chars: List[Char]): Boolean = {
    def accepts_r(chars: List[Char], currentStates: Set[State]): Boolean = {
      if (chars.size == 1)
        this.transitionAndEpsilonJump(currentStates, chars.head).intersect(this.finalStates).size > 0
      else
        accepts_r(chars.tail, this.transitionAndEpsilonJump(currentStates, chars.head))
    }

    if (chars.size == 0)
      this.epsilonJump(this.startState).intersect(this.finalStates).size > 0
    else
      accepts_r(chars, Set(this.startState))
  }

  def isDeterministic(): Boolean = {
    //no state has epsilon jumps
    this.transitions.forall(transition => !(transition._1._2 == '\0')) &&
      //every state has the same alphabet as the initial state
      this.states.forall(state => this.transitions.filter(_._1._1 == state).map(_._1._2).toSet == this.alphabet) &&
      //each state has no more than one transition per character
      this.states.forall(state => this.alphabet.forall(letter => this.transitions.filter(transition => transition._1._1 == state && transition._1._2 == letter).size == 1))
  }

  //  bug with method:
  //  (a union Set(b,c,d)).removeUnreachableStates.removeNondisinguishableStates.removeDeadStates.print
//  def removeNondistinguishableStates(): Automaton = {
//
//  }
  
  def getDFA(): Automaton = this.getRelativeDFA(this.alphabet)

  def getRelativeDFA(alphabet: Set[Char]): Automaton = {
    def getRelativeDFA_r(automaton: Automaton): Automaton = {
      if (automaton.states.forall(state => automaton.transitions.filter(_._1._1 == state).size > 0)) {
        automaton
      } else {
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

    val newStart = new State(this.epsilonJump(this.startState))
    getRelativeDFA_r(new Automaton(newStart, if (this.finalStates.intersect(newStart.associatedStates).size > 0) Set(newStart) else Set(), Set()))
  }

  //  def isFinite(): Boolean = this.minimize.states.forall(state => !pathExists(state, state))
  def isFinite(): Boolean = this.states.forall(state => !pathExists(state, state))
  def isEmpty(): Boolean = this.finalStates.size == 0
  def isTotal(): Boolean = this.complement.isEmpty

  def equals(automaton: Automaton): Boolean = {
    ((this intersect (automaton.complement)) union ((this.complement) intersect automaton)).isEmpty
  }
  def ==(automaton: Automaton): Boolean = this.equals(automaton)

  def isSubsetOf(automaton: Automaton): Boolean = {
    (this intersect automaton) == this
  }

  def substitute(oldChar: Char, newChar: Char): Automaton = new Automaton(this.startState, this.finalStates, this.transitions.map(transition => if (transition._1._2 == oldChar) ((transition._1._1, newChar) -> transition._2) else transition))

  def pathExists(stateFrom: State, stateTo: State): Boolean = this.getReachableStates(stateFrom).contains(stateTo)

  def getConnectedStates(state: State): Set[State] = this.transitions.filter(_._1._1 == state).map(_._2).toSet
  def getConnectedStates(states: Set[State]): Set[State] = states.flatMap(this.getConnectedStates(_))

  def getReachableStates(state: State): Set[State] = getReachableStates(Set(state))
  def getReachableStates(states: Set[State]): Set[State] = {
    val allStates = states.union(this.getConnectedStates(states))
    if (allStates.subsetOf(states))
      states
    else
      getReachableStates(allStates)
  }
}