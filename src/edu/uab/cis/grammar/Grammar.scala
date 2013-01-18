package edu.uab.cis.grammar

import edu.uab.cis.automaton.State

class Grammar(val startVariable: String, val rules: Set[(String, List[Any])]) {

  val variables: Set[String] = rules.map(_._1)
  val alphabet: Set[Char] = rules.flatMap(rule => rule._2.filter(_.isInstanceOf[Char]).map(_.asInstanceOf[Char]))

  lazy val pda: PDA = {
    val startState = new State
    val state = new State
    val startRule = ((startState, '\0', '\0') -> (state, List(startVariable)))
    val endRule = ((state, '\0', '\0') -> (new State, List('\0')))
    
    new PDA(
      startRule,
      alphabet.map(letter => {
        ((state, letter, letter) -> (state, List('\0')))
      }) ++
        rules.map(rule => {
          ((state, '\0', rule._1) -> (state, rule._2))
        }) + startRule + endRule)
  }

  //  private def firstUnusedId(ids: Set[Int]): Int = {
  //    def firstUnusedId_r(counter: Int): Int = {
  //      if (ids contains counter)
  //        firstUnusedId_r(counter + 1)
  //      else
  //        counter
  //    }
  //    firstUnusedId_r(1)
  //  }

  override def toString(): String = {
    //print start rules
    rules.filter(_._1 == startVariable).map(rule => {
      (rule._1 + " -> ") +
        rule._2.map(result => {
          result match {
            case int: Int => "(" + result.toString + ")"
            case string: String => "\"" + result + "\""
            case None => "(empty)"
          }
        }).reduce(_ + _) + "\n"
    }).reduce(_ + _) +
      //print other rules
      rules.filterNot(_._1 == startVariable).map(rule => {
        (rule._1 + " -> ") +
          rule._2.map(result => {
            result match {
              case int: Int => "(" + result.toString + ")"
              case string: String => "\"" + result + "\""
              case None => "(empty)"
            }
          }).reduce(_ + _) + "\n"
      }).reduce(_ + _)
  }

  implicit def grammar2PDA(): PDA = pda
  
  def toPDAString(): String = pda.toString()
}

