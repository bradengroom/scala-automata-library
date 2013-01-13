package edu.uab.cis.automaton;

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
    new Automaton(state1, Set(state2), Set(((state1, char), state2)))
  }

  def a(): Automaton = char('a')
  def b(): Automaton = char('b')
  def c(): Automaton = char('c')
  def d(): Automaton = char('d')
  def e(): Automaton = char('e')
  def f(): Automaton = char('f')
  def g(): Automaton = char('g')
  def h(): Automaton = char('h')
  def i(): Automaton = char('i')
  def j(): Automaton = char('j')
  def k(): Automaton = char('k')
  def l(): Automaton = char('l')
  def m(): Automaton = char('m')
  def n(): Automaton = char('n')
  def o(): Automaton = char('o')
  def p(): Automaton = char('p')
  def q(): Automaton = char('q')
  def r(): Automaton = char('r')
  def s(): Automaton = char('s')
  def t(): Automaton = char('t')
  def u(): Automaton = char('u')
  def v(): Automaton = char('v')
  def w(): Automaton = char('w')
  def x(): Automaton = char('x')
  def y(): Automaton = char('y')
  def z(): Automaton = char('z')
  def A(): Automaton = char('A')
  def B(): Automaton = char('B')
  def C(): Automaton = char('C')
  def D(): Automaton = char('D')
  def E(): Automaton = char('E')
  def F(): Automaton = char('F')
  def G(): Automaton = char('G')
  def H(): Automaton = char('H')
  def I(): Automaton = char('I')
  def J(): Automaton = char('J')
  def K(): Automaton = char('K')
  def L(): Automaton = char('L')
  def M(): Automaton = char('M')
  def N(): Automaton = char('N')
  def O(): Automaton = char('O')
  def P(): Automaton = char('P')
  def Q(): Automaton = char('Q')
  def R(): Automaton = char('R')
  def S(): Automaton = char('S')
  def T(): Automaton = char('T')
  def U(): Automaton = char('U')
  def V(): Automaton = char('V')
  def W(): Automaton = char('W')
  def X(): Automaton = char('X')
  def Y(): Automaton = char('Y')
  def Z(): Automaton = char('Z')
}