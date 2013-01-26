package edu.uab.cis.contextfree

import edu.uab.cis._

class CFG(val startVariable: String, val rules: Set[(String, List[Any])]) {

  type Rule = (String, List[Any])

  val variables: Set[String] = rules.map(_._1)
  val alphabet: Set[Char] = rules.flatMap(rule => rule._2.filter(_.isInstanceOf[Char]).map(_.asInstanceOf[Char]))

  def union(cfg: CFG): CFG = {
    new CFG("S",
      this.rules.map(rule => {
        (rule._1 + "1", rule._2.map(result => {
          result match {
            case string: String => string + "1"
            case char: Char => char
          }
        }))
      }) ++
        cfg.rules.map(rule => {
          (rule._1 + "2", rule._2.map(result => {
            result match {
              case string: String => string + "2"
              case char: Char => char
            }
          }))
        }) +
        (("S", List(this.startVariable + "1"))) +
        (("S", List(cfg.startVariable + "2"))))
  }
  def |(cfg: CFG) = this.union(cfg)

  def concatenate(cfg: CFG): CFG = {
    new CFG("S",
      this.rules.map(rule => {
        (rule._1 + "1", rule._2.map(result => {
          result match {
            case string: String => string + "1"
            case char: Char => char
          }
        }))
      }) ++
        cfg.rules.map(rule => {
          (rule._1 + "2", rule._2.map(result => {
            result match {
              case string: String => string + "2"
              case char: Char => char
            }
          }))
        }) +
        (("S", List(this.startVariable + "1", cfg.startVariable + "Y"))))
  }
  def +(cfg: CFG) = this.concatenate(cfg)

  def repeat(): CFG = {
    new CFG("S",
      this.rules.map(rule => {
        (rule._1 + "1", rule._2.map(result => {
          result match {
            case string: String => string + "1"
            case char: Char => char
          }
        }))
      }) +
        (("S", List(this.startVariable + "1", "S"))) +
        (("S", List('\0'))))
  }
  def *() = repeat()

  lazy val pda: PDA = {
    val startState = new State
    val state = new State
    val finalState = new State
    val startRule = ((startState, '\0', '\0') -> (state, List(startVariable)))
    val endRule = ((state, '\0', '\0') -> (finalState, List('\0')))

    new PDA(
      startRule,
      Set(finalState),
      alphabet.map(letter => {
        ((state, letter, letter) -> (state, List('\0')))
      }) ++
        rules.map(rule => {
          ((state, '\0', rule._1) -> (state, rule._2))
        }) + startRule + endRule)
  }

  override def toString(): String = {
    def toString_r(rule: Rule): String = { //(String, List[Any])
      rule._1 + " -> " +
        rule._2.map(result => {
          result match {
            case string: String => "(" + result.toString + ")"
            case char: Char => "\"" + (if (result == '\0') "ϵ" else result) + "\""
          }
        }).reduce(_ + _) + "\n"
    }
    //print start rules
    rules.filter(_._1 == startVariable).map(rule => {
      toString_r(rule)
    }).reduce(_ + _) +
      //print other rules
      rules.filterNot(_._1 == startVariable).map(rule => {
        toString_r(rule)
      }).reduce(_ + _)
  }
}

