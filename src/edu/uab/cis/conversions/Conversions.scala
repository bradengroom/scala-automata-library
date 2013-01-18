package edu.uab.cis.conversions

import edu.uab.cis.automaton._
import edu.uab.cis.cfg._

object Conversions {

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