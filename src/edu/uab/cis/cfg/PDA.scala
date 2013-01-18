package edu.uab.cis.cfg

import edu.uab.cis.automaton.State

class PDA(val startRule: ((State, Char, Any), (State, List[Any])), val finalStates: Set[State], val rules: Set[((State, Char, Any), (State, List[Any]))]) {
  type Rule = ((State, Char, Any), (State, List[Any]))

  override def toString(): String = {
    def ruleString(rule: Rule): String = {
      "d(" + rule._1._1.getId + ", " +
        (if (rule._1._2 == '\0') "ϵ" else rule._1._2) + ", " +
        (if (rule._1._3 == '\0') "ϵ" else rule._1._3) + ") = (" +
        rule._2._1.getId + ", " +
        rule._2._2.map(letter => {
          if (letter == '\0') "ϵ" else letter.toString
        }).reduce(_ + _) + ")"
    }

    ruleString(startRule) + "\n" +
      (rules - startRule).map(rule => {
        ruleString(rule)
      }).reduce(_ + "\n" + _)
  }
}