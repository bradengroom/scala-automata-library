package edu.uab.cis.automaton;
private object State {
  private var currentId = 0
  private def getNextId = { currentId += 1; currentId }
}

@serializable
class State(val associatedStates: Set[State]) {

  def this() = this(null)

  private val id = State.getNextId
  def getId(): Int = id
}