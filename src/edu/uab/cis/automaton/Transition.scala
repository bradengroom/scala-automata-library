package edu.uab.cis.automaton;
class Transition(val end: State, val char: Char, val isGeneralized: Boolean, val regex: String) {

  def this(end: State) = this(end, '\0', false, null)
  def this(end: State, char: Char) = this(end, char, false, null)
  def this(end: State, regex: String) = this(end, '\0', true, regex)

  def |(transition: Transition): Transition = {
    new Transition(transition.end, "(" + this.getLabel + "|" + transition.getLabel + ")")
    this
  }

  def getLabel(): String = {
    if (this.isGeneralized) {
      regex
    } else {
      char.toString
    }
  }

  def print() = {
    if (char == '\0') {
      println("	-> " + end.getId)
    } else {
      println("	" + char + "-> " + end.getId)
    }
  }
}