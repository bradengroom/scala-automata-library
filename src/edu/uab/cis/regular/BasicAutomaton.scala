package edu.uab.cis.regular

import edu.uab.cis.State

object BasicAutomaton {

  /**
   * @param beginChar
   * @param endChar
   * @return Returns an automaton that accepts a characters from beginChar to endChar
   */
  def range(beginChar: Char, endChar: Char): Automaton = {
    if (beginChar > endChar)
      empty()
    else if (beginChar equals endChar)
      char(beginChar)
    else
      char(beginChar) union range((beginChar + 1).toChar, endChar)
  }

  /**
   * @return Returns an automaton that accepts no strings
   */
  def empty(): Automaton = new Automaton(new State(), Set(), Set())

  /**
   * @return Returns an automaton that accepts the empty string
   */
  def emptyString(): Automaton = {
    val startState = new State
    new Automaton(startState, Set(startState), Set())
  }

  /**
   * @param alphabet
   * @return Returns an automaton that accepts all strings that can be created with the given alphabet
   */
  def total(alphabet: Set[Char]): Automaton = empty.relativeComplement(alphabet)

  /**
   * @param string
   * @return Returns an automaton that accepts the given string
   */
  def string(string: String): Automaton = string.toList.map(char(_)).reduceRight(_ + _)

  /**
   * @param char
   * @return Returns an automaton that accepts the given character
   */
  def char(char: Char): Automaton = {
    val state1 = new State()
    val state2 = new State()
    new Automaton(state1, Set(state2), Set((state1, char, state2)))
  }

}