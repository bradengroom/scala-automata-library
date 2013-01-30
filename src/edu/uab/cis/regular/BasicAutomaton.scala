package edu.uab.cis.regular

import edu.uab.cis.State
import scala.annotation.switch
//case class OnePlusChar(x: Option[Char]) { def ==(c: Char) = x forall (_ == c) }
//implicit def FromOnePlusChar(c: Char) = OnePlusChar(Some(c))
object BasicAutomaton {

  /**
   * @param beginChar
   * @param endChar
   * @return Returns an automaton that accepts a characters from beginChar to endChar
   */
  def range(beginChar: Char, endChar: Char): Automaton = {
    val startState = new State()
    val finalState = new State()
    val transitions: Set[(State, Char, State)] = (beginChar to endChar).map(char => (startState, char, finalState)).toSet
    new Automaton(startState, Set(finalState), transitions)
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

  /**
   * @param regexString
   * @return Returns the provided regular expression as an automaton
   */
  def regex(regexString: String): Automaton = {
    def regex_r(regexString: String, automata: List[Automaton]): List[Automaton] = {
      if (regexString.isEmpty) {
        automata
      } else (regexString.head: @switch) match {
        case '(' => regex_r(regexString.substring(getMatchingMarker('(', ')', regexString)), automata ++ List(regex(regexString.substring(1, getMatchingMarker('(', ')', regexString) - 1))))
        case '[' => {
          val rangeChars = regexString.substring(1, getMatchingMarker('[', ']', regexString) - 1)
          regex_r(regexString.substring(getMatchingMarker('[', ']', regexString)), automata ++ List(range(rangeChars.head, rangeChars.last)))
        }
        case '{' => {
          val repetionChars = regexString.substring(1, getMatchingMarker('{', '}', regexString) - 1).replaceAll(" ", "").split(",")
          if (repetionChars.size == 1) {
            regex_r(regexString.substring(getMatchingMarker('{', '}', regexString)), automata.init ++ List(automata.last.repeat(repetionChars.head.toInt)))
          } else {
            regex_r(regexString.substring(getMatchingMarker('{', '}', regexString)), automata.init ++ List(automata.last.repeat(repetionChars.head.toInt, repetionChars.last.toInt)))
          }
        }
        case '+' => {
          if (regexString.tail.isEmpty) {
            automata.init ++ List(automata.last+)
          } else {
            List(automata.reduce(_ + _) + regex(regexString.tail))
          }
        }
        case '|' => List(automata.reduce(_ + _) | regex(regexString.tail))
        case '&' => List(automata.reduce(_ + _) & regex(regexString.tail))
        case '-' => {
          val followingAutomata = regex_r(regexString.tail, List())
          automata.init ++ List(automata.last - followingAutomata.head) ++ followingAutomata.tail
        }
        case '*' => regex_r(regexString.tail, automata.init ++ List(automata.last.repeat))
        case '?' => regex_r(regexString.tail, automata.init ++ List(automata.last.optional))
        case '\\' => regex_r(regexString.tail.tail, automata ++ List(char(regexString.tail.head)))
        case _ => regex_r(regexString.tail, automata ++ List(char(regexString.head)))
      }
    }

    regex_r(regexString, List()).reduce(_ + _)
  }

  private def getMatchingMarker(openMarker: Char, closeMarker: Char, string: String): Int = {
    def getMatchingMarker_r(string: String, count: Int = 1): Int = {
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
    string.size - getMatchingMarker_r(string.tail)
  }

}