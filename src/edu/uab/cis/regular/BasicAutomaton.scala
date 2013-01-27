package edu.uab.cis.regular

import edu.uab.cis.State
//case class OnePlusChar(x: Option[Char]) { def ==(c: Char) = x forall (_ == c) }
//implicit def FromOnePlusChar(c: Char) = OnePlusChar(Some(c))
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

  def regex(regexString: String): Automaton = {

    def regex_r(regexString: String, automata: List[Automaton]): List[Automaton] = {
      if (regexString.isEmpty) {
        automata
      } else if (regexString.head == '(') {
        regex_r(regexString.substring(getMatchingMarker('(', ')', regexString)), automata ++ List(regex(regexString.substring(1, getMatchingMarker('(', ')', regexString) - 1))))
      } else if (regexString.head == '[') {
        val rangeChars = regexString.substring(1, getMatchingMarker('[', ']', regexString) - 1)
        regex_r(regexString.substring(getMatchingMarker('[', ']', regexString)), automata ++ List(range(rangeChars.head, rangeChars.charAt(2))))
      } else if (regexString.head == '+') {
        val followingAutomata = regex_r(regexString.tail, List())
        automata.init ++ List(automata.last + followingAutomata.head) ++ followingAutomata.tail
      } else if (regexString.head == '|') {
        val followingAutomata = regex_r(regexString.tail, List())
        automata.init ++ List(automata.last | followingAutomata.head) ++ followingAutomata.tail
      } else if (regexString.head == '&') {
        val followingAutomata = regex_r(regexString.tail, List())
        automata.init ++ List(automata.last & followingAutomata.head) ++ followingAutomata.tail
      } else if (regexString.head == '-') {
        val followingAutomata = regex_r(regexString.tail, List())
        automata.init ++ List(automata.last - followingAutomata.head) ++ followingAutomata.tail
      } else if (regexString.head == '*') {
        regex_r(regexString.tail, automata.init ++ List(automata.last.repeat))
      } else if (regexString.head == '?') {
        regex_r(regexString.tail, automata.init ++ List(automata.last.optional))
      } else if (regexString.head == '\\') {
        regex_r(regexString.tail.tail, automata ++ List(char(regexString.tail.head)))
      } else {
        regex_r(regexString.tail, automata ++ List(char(regexString.head)))
      }
    }

    regex_r(regexString, List()).reduce(_ + _)
  }

  def getMatchingMarker(openMarker: Char, closeMarker: Char, string: String): Int = {
    def getMatchingMarker_r(string: String, count: Int): Int = {
      if (count == 0) {
        string.size
      } else if (string.head == closeMarker) {
        getMatchingMarker_r(string.tail, count - 1)
      } else if (string.head == openMarker) {
        getMatchingMarker_r(string.tail, count + 1)
      } else if (string.startsWith("\\" + closeMarker) || string.startsWith("\\" + openMarker)) {
        getMatchingMarker_r(string.substring(2), count)
      } else {
        getMatchingMarker_r(string.tail, count)
      }
    }
    string.size - getMatchingMarker_r(string.tail, 1)
  }

}