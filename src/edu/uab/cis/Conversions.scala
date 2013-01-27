package edu.uab.cis

import edu.uab.cis.regular._
import edu.uab.cis.contextfree._

object Conversions {

  implicit def String2Automaton(regex: String): Automaton = BasicAutomaton.regex(regex)
  
  /**
   * @return Returns the automaton as a CFG
   */
  implicit def Automaton2CFG(automaton: Automaton): CFG = {
    new CFG(automaton.startState.getId.toString,
      automaton.transitions.map(transition => {
        transition._1.getId.toString -> List(transition._2, transition._3.getId.toString)
      }) ++
        automaton.finalStates.map(finalState => {
          finalState.getId.toString -> List('\0')
        }))
  }

  implicit def CFG2PDA(cfg: CFG): PDA = cfg.pda
}