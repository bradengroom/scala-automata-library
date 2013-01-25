import edu.uab.cis.regular._
import edu.uab.cis.contextfree._
import edu.uab.cis._

object Test {

  def main(args: Array[String]) = {

    val auto = char('a')
    println(auto)


  }

  def char(char: Char): Automaton = {
    val state1 = new State()
    val state2 = new State()
    new Automaton(state1, Set(state2), Set((state1, char, state2)))
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