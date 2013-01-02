package edu.uab.cis.automaton;
private object State{
    private var currentId = 0
    private def getNextId = {currentId += 1; currentId}
}

class State(val associatedStates: Set[State]) {
  
  def this() = this(null)
  
  private var is_final: Boolean = false
  private var is_initial = false
  private var transitions: Set[Transition] = Set()
  private val id = State.getNextId

  def isFinal = this.is_final
  def setFinal(value: Boolean) = this.is_final = value

  def isInitial = this.is_initial
  def setInitial(value: Boolean) = this.is_initial = value

  def addTransition(transition: Transition) = this.transitions += transition
  def addTransitions(transitions: Set[Transition]) = this.transitions ++= transitions
  def removeTransition(transition: Transition) = this.transitions -= transition
  def removeTransitions(transitions: Set[Transition]) = this.transitions --= transitions

  def getAlphabet(): Set[Char] = this.getTransitions().map(_.char) diff Set('\0')

  def complement(): State = {
    val complementState: State = this
    complementState.setFinal(!this.is_final)
    complementState
  }

  def print() = {
    if (is_initial && is_final) {
      println("State " + id + " [initial][final]:")
    } else if (is_initial) {
      println("State " + id + " [initial]:")
    } else if (is_final) {
      println("State " + id + " [final]:")
    } else {
      println("State " + id + ":")
    }
    transitions.foreach(_.print)
  }

  def getTransitions(): Set[Transition] = transitions
  def getId(): Int = id

  def getNextStates(char: Char): Set[State] = {
    this.transitions.filter(_.char == char).map(_.end)
  }

  def getNextStates(): Set[State] = getNextStates('\0') + this

  private def getConnectedStates(): Set[State] = this.transitions.map(_.end)

  def hasPathTo(state: State): Boolean = {

    def hasPathTo_r(state: State, states: Set[State]): Boolean = {
      val connectedStates = states.flatMap(_.getConnectedStates) union states
      if ((connectedStates contains state) || states == connectedStates) {
        connectedStates contains state
      } else {
        hasPathTo_r(state, connectedStates)
      }
    }

    val connectedStates = this.getConnectedStates
    if (connectedStates contains state) {
      true
    } else {
      hasPathTo_r(state, connectedStates)
    }
  }

  def hasEpsilonJumps(): Boolean = this.getTransitions().map(_.char).contains('\0')
}