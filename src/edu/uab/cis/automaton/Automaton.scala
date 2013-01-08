package edu.uab.cis.automaton;

class Automaton(var states: Set[State]) {

  def this() = this(Set())

  //add and remove states
  def addState(state: State) = { this.states += state }
  def removeState(state: State) = { this.states -= state }
  def addStates(states: Set[State]) = { this.states ++= states }
  def removeStates(states: Set[State]) = { this.states --= states }

  def complement(): Automaton = new Automaton(this.getDFA.states.map((state: State) => state.complement))

//  def relativeComplement(alphabet: Set[Char]): Automaton = new Automaton(this.getRelativeDFA(alphabet).states.map((state: State) => state.complement))
//  def relativeComplement(automaton: Automaton): Automaton = this.complement.intersect(automaton)

  def union(automaton: Automaton): Automaton = {
    //new start state
    val startState = new State()
    startState.setInitial(true)

    //combine states
    val unionedStates = this.states union automaton.states

    //create transition from new start to old starts
    unionedStates.filter(_.isInitial).foreach((state: State) => {
      state.setInitial(false)
      startState.addTransition(new Transition(state))
    })

    new Automaton(unionedStates + startState)
  }
  def |(automaton: Automaton): Automaton = union(automaton)

  def union(automata: Set[Automaton]): Automaton = {
    (automata).reduceRight(_ | _) | this
  }

  def repeat(): Automaton = {

    //new start state
    val startState = new State()
    startState.setFinal(true)
    startState.setInitial(true)

    //transition old finals to old start
    this.getFinalStates.foreach((state: State) => {
      state.addTransition(new Transition(this.getInitialState))
    })

    //transition new start to old start
    startState.addTransition(new Transition(this.getInitialState))
    this.getInitialState.setInitial(false)

    new Automaton(states + startState)
  }
  def *() = repeat()

  //needs more testing
  def intersect(automaton: Automaton): Automaton = {
    val fullAlphabet = (this.getAlphabet union automaton.getAlphabet)
    //De Morgan's Law
    ((this.getRelativeDFA(fullAlphabet).complement) | (automaton.getRelativeDFA(fullAlphabet).complement)).complement
  }

  def concatenate(automaton: Automaton): Automaton = {

    //transition final states of first to initial state of second
    this.getFinalStates.foreach((state: State) => {
      state.setFinal(false)
      state.addTransition(new Transition(automaton.getInitialState))
    })

    //set intial of first not to be initial
    automaton.getInitialState.setInitial(false)
    new Automaton(this.states union automaton.states)
  }
  def +(automaton: Automaton) = concatenate(automaton)

  def concatenate(automata: Set[Automaton]): Automaton = {
    this + (automata).reduceRight(_ + _)
  }
  def ++(automata: Set[Automaton]): Automaton = this.concatenate(automata)

  def minus(automaton: Automaton): Automaton = {
    this.intersect(automaton.complement)
  }
  def -(automaton: Automaton): Automaton = this.minus(automaton)

  def optional(): Automaton = {
    val newStart = new State
    newStart.setInitial(true)
    newStart.setFinal(true)

    val newEnd = new State
    newEnd.setInitial(true)
    newEnd.setFinal(true)

    val automaton = (new Automaton(Set(newStart)) + this + new Automaton(Set(newEnd)))
    automaton.getInitialState.addTransition(new Transition(automaton.getFinalStates.head))
    automaton
  }
  def ?(): Automaton = this.optional()

  def print() = {
    //print initial
    //this.getInitialState.print()
    //print others
    //this.states.filterNot(_.isInitial).foreach(_.print())
    this.states.foreach(_.print())
  }

  def getInitialState(): State = this.states.filter(_.isInitial).head
  def getFinalStates(): Set[State] = this.states.filter(_.isFinal)
  def getAlphabet(): Set[Char] = this.states.flatMap(_.getAlphabet)

  def getStateCount(): Int = this.states.size
  def getTransitionCount(): Int = this.states.flatMap(_.getTransitions).size

  def getNextStates(states: Set[State], char: Char): Set[State] = {
    states.flatMap(_.getNextStates(char))
  }

  def getNextStates(currentStates: Set[State]): Set[State] = {
    val nextStates = currentStates.flatMap(_.getNextStates)
    if (currentStates == (currentStates union nextStates)) {
      currentStates
    } else {
      getNextStates(currentStates union nextStates)
    }
  }

  def accepts(string: String): Boolean = {

    def accepts_r(chars: List[Char], currentStates: Set[State]): Boolean = {
      if (chars.size > 1) {
        accepts_r(chars.tail, this.getNextStates(this.getNextStates(this.getNextStates(currentStates), chars.head)))
      } else {
        !this.getNextStates(this.getNextStates(this.getNextStates(currentStates), chars.head)).forall(!_.isFinal)
      }
    }

    if (string.toList.size == 0) {
      !this.getNextStates(Set(this.getInitialState)).forall(!_.isFinal)
    } else if (string.toList.size > 1) {
      accepts_r(string.toList.tail, this.getNextStates(this.getNextStates(Set(this.getInitialState())), string.toList.head))
    } else {
      !this.getNextStates(this.getNextStates(this.getNextStates(Set(this.getInitialState())), string.toList.head)).forall(!_.isFinal)
    }
  }

  def isDeterministic(): Boolean = {
    val alphabetToMatch = this.getInitialState.getAlphabet
    //no state has epsilon jumps
    states.forall(!_.hasEpsilonJumps) &&
      //every state has the same alphabet as the initial state
      states.forall(_.getAlphabet == alphabetToMatch) &&
      //each state has no more than one transition per character
      states.forall((state: State) => state.getAlphabet.forall((letter: Char) => state.getTransitions.filter(_.char == letter).size == 1))
  }

  def removeUnreachableStates(): Automaton = {
    new Automaton(this.states.filter((state: State) => {
      state.isInitial || (this.getInitialState.hasPathTo(state))
    }).map((state: State) => {
      state.removeTransitions(state.getTransitions().filterNot(states contains _.end))
      state
    }))
  }

  def removeDeadStates(): Automaton = {
    val newStates = this.states.filter((state: State) => { state.isFinal || !this.getFinalStates.forall(!state.hasPathTo(_)) })
    newStates.foreach(state => state.removeTransitions(state.getTransitions.filter(transition => !newStates.contains(transition.end))))
    new Automaton(newStates)
  }

  //bug with method:
  //(a union Set(b,c,d)).removeUnreachableStates.removeNondisinguishableStates.removeDeadStates.print
  def removeNondistinguishableStates(): Automaton = {

    def removeIndistiguishableStates_r(): Automaton = {
      val originalStates = this.states
      val statePairsToCheck: Set[List[State]] = (this.states.toList.map(_ => this.states.toList)).flatten.combinations(2).toSet.filter((set: List[State]) => { set(0) != set(1) && ((set(0).isFinal && set(1).isFinal) || (!set(0).isFinal && !set(1).isFinal)) })

      statePairsToCheck.filter((list: List[State]) => {
        this.getAlphabet.forall((letter: Char) => {
          this.getNextStates(Set(list(0)), letter) == this.getNextStates(Set(list(1)), letter)
        })
      }).foreach((pair: List[State]) => {
        // mergedState.setInitial(pair(0).isInitial || pair(1).isInitial)

        this.states.foreach((state: State) => {

          state.getTransitions.foreach((transition: Transition) => {

            if (pair(1) == transition.end) {
              state.addTransition(new Transition(pair(0), transition.char))
              state.removeTransition(transition)
            }
          })
        })

        pair(0).setInitial(pair(0).isInitial || pair(1).isInitial)

        this.states -= pair(1)
      })

      if (states == originalStates) {
        this
      } else {
        removeIndistiguishableStates_r()
      }
    }

    if (!this.isDeterministic) {
      this.getDFA.removeNondistinguishableStates
    } else {
      removeIndistiguishableStates_r
    }
  }

  def getDFA(): Automaton = {

    this.getRelativeDFA(this.getAlphabet)
  }

  def getRelativeDFA(alphabet: Set[Char]): Automaton = {

    def getDFA_r(automaton: Automaton, alphabet: Set[Char]): Automaton = {
      automaton.states.filter(_.getTransitions.size == 0).foreach(state => {
        alphabet.foreach(letter => {
          val nextStatesOnChar = this.getNextStates(this.getNextStates(this.getNextStates(state.associatedStates), letter))
          if (automaton.states.filter(_.associatedStates == nextStatesOnChar).size > 0) {
            state.addTransition(new Transition(automaton.states.filter(_.associatedStates == nextStatesOnChar).head, letter))
          } else {
            val newState = new State(nextStatesOnChar)
            newState.setFinal(nextStatesOnChar.filter(_.isFinal).size > 0)
            state.addTransition(new Transition(newState, letter))
            automaton.addState(newState)
          }
        })
      })

      if (automaton.states.forall(_.getTransitions.size == alphabet.size)) {
        automaton
      } else {
        getDFA_r(automaton, alphabet)
      }
    }

    val associatedStates = this.getNextStates(Set(this.getInitialState))
    val newStartState = new State(associatedStates)

    newStartState.setInitial(true)
    newStartState.setFinal(!associatedStates.forall(!_.isFinal))
    getDFA_r(new Automaton(Set(newStartState)), alphabet)
  }

  def minimize(): Automaton = {
    this.removeUnreachableStates.removeNondistinguishableStates.removeDeadStates
  }

  def isFinite(): Boolean = {
    this.minimize.states.forall(state => !state.hasPathTo(state))
  }

  def isEmpty(): Boolean = {
    this.removeUnreachableStates.states.filter(_.isFinal).size == 0
  }

  def isTotal(): Boolean = {
    this.complement.isEmpty
  }

  def equals(automaton: Automaton): Boolean = {
    ((this intersect (automaton.complement)) union ((this.complement) intersect automaton)).isEmpty
  }
  def ==(automaton: Automaton): Boolean = this.equals(automaton)

  def isSubsetOf(automaton: Automaton): Boolean = {
    (this intersect automaton) == this
  }

  def substitute(oldChar: Char, newChar: Char): Automaton = {
    this.states.foreach(state => {
      state.getTransitions.filter(_.char == oldChar).foreach(transition => {
          state.addTransition(new Transition(transition.end, newChar))
          state.removeTransition(transition)
      })
    })
    this
  }

  //  def toRegex(): String = {
  //
  //    val newStart = new State
  //    newStart.setInitial(true)
  //    newStart.setFinal(true)
  //
  //    val newEnd = new State
  //    newEnd.setInitial(true)
  //    newEnd.setFinal(true)
  //
  //    val automaton = (new Automaton(Set(newStart)) + this + new Automaton(Set(newEnd))).minimize
  //
  //    automaton.toRegex_r
  //  }
  //
  //  def toRegex_r(): String = {
  //
  //    states.foreach(state => {
  //      states.foreach(stateToMatch => {
  //        if (state.getTransitions.filter(_.end == stateToMatch).size > 1) {
  //          val transitionsToRemove = state.getTransitions.filter(_.end == stateToMatch)
  //          println(transitionsToRemove.reduceRight(_ | _).getLabel)
  //          state.removeTransitions(transitionsToRemove)
  //        }
  //      })
  //    })
  //
  //    new String
  //  }
}