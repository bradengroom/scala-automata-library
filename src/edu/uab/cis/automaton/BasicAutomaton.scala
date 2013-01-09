package edu.uab.cis.automaton;

object BasicAutomaton {

  def basicAutomaton(char: Char): Automaton = {
    val state1 = new State()
    state1.setInitial(true)
    val state2 = new State()
    state2.setFinal(true)
    val transition = new Transition(state2, char)
    state1.addTransition(transition)
    new Automaton(Set(state1, state2))
  }

  def range(begin: Char, end: Char): Automaton = {
    if (begin > end)
      emptyAutomaton()
    else if (begin equals end)
      basicAutomaton(begin)
    else
      basicAutomaton(begin) union range((begin + 1).toChar, end)
  }

  def emptyAutomaton(): Automaton = {
    val state = new State()
    state.setInitial(true)
    new Automaton(Set(state))
  }

  def a(): Automaton = basicAutomaton('a')
  def b(): Automaton = basicAutomaton('b')
  def c(): Automaton = basicAutomaton('c')
  def d(): Automaton = basicAutomaton('d')
  def e(): Automaton = basicAutomaton('e')
  def f(): Automaton = basicAutomaton('f')
  def g(): Automaton = basicAutomaton('g')
  def h(): Automaton = basicAutomaton('h')
  def i(): Automaton = basicAutomaton('i')
  def j(): Automaton = basicAutomaton('j')
  def k(): Automaton = basicAutomaton('k')
  def l(): Automaton = basicAutomaton('l')
  def m(): Automaton = basicAutomaton('m')
  def n(): Automaton = basicAutomaton('n')
  def o(): Automaton = basicAutomaton('o')
  def p(): Automaton = basicAutomaton('p')
  def q(): Automaton = basicAutomaton('q')
  def r(): Automaton = basicAutomaton('r')
  def s(): Automaton = basicAutomaton('s')
  def t(): Automaton = basicAutomaton('t')
  def u(): Automaton = basicAutomaton('u')
  def v(): Automaton = basicAutomaton('v')
  def w(): Automaton = basicAutomaton('w')
  def x(): Automaton = basicAutomaton('x')
  def y(): Automaton = basicAutomaton('y')
  def z(): Automaton = basicAutomaton('z')
  def A(): Automaton = basicAutomaton('A')
  def B(): Automaton = basicAutomaton('B')
  def C(): Automaton = basicAutomaton('C')
  def D(): Automaton = basicAutomaton('D')
  def E(): Automaton = basicAutomaton('E')
  def F(): Automaton = basicAutomaton('F')
  def G(): Automaton = basicAutomaton('G')
  def H(): Automaton = basicAutomaton('H')
  def I(): Automaton = basicAutomaton('I')
  def J(): Automaton = basicAutomaton('J')
  def K(): Automaton = basicAutomaton('K')
  def L(): Automaton = basicAutomaton('L')
  def M(): Automaton = basicAutomaton('M')
  def N(): Automaton = basicAutomaton('N')
  def O(): Automaton = basicAutomaton('O')
  def P(): Automaton = basicAutomaton('P')
  def Q(): Automaton = basicAutomaton('Q')
  def R(): Automaton = basicAutomaton('R')
  def S(): Automaton = basicAutomaton('S')
  def T(): Automaton = basicAutomaton('T')
  def U(): Automaton = basicAutomaton('U')
  def V(): Automaton = basicAutomaton('V')
  def W(): Automaton = basicAutomaton('W')
  def X(): Automaton = basicAutomaton('X')
  def Y(): Automaton = basicAutomaton('Y')
  def Z(): Automaton = basicAutomaton('Z')

}