/**
 * A literal is an atom or its negation.
 */
sealed abstract class Literal {
  /**
    * Returns the negation of the literal.
    * @return The negation of the literal.
    */
  def neg: Literal = {
    this match {
      case atom@Atom(_) => Neg(atom)
      case Neg(atom) => atom
    }
  }.ensuring(_ != this)

  /**
    * Returns the positive form of the literal.
    * @return The positive form of the literal.
    */
  def positive: Atom = {
    this match {
      case atom@Atom(_) => atom
      case Neg(atom) => atom
    }
  }

  override def toString: String = {
    this match {
      case Atom(name) => name
      case Neg(Atom(name)) => "¬" + name
    }
  }
}

case class Atom(val name: String) extends Literal { require(name != "") }
case class Neg(val atom: Atom) extends Literal
