/**
 * An atom is a literal or a negated literal.
 */
sealed abstract class Atom {
  /**
    * Returns the negation of this atom.
    * @return The negation of this atom.
    */
  def neg: Atom = {
    this match {
      case l@Lit(name) => Neg(l)
      case Neg(l) => l
    }
  }.ensuring(_ != this)

  /**
    * Returns the literal representation of this atom.
    * @return The literal representation of this atom.
    */
  def asLit: Lit = {
    this match {
      case l@Lit(name) => l
      case Neg(l) => l
    }
  }

  override def toString: String = {
    this match {
      case Lit(name) => name
      case Neg(l: Lit) => "¬" + l.name
    }
  }
}

case class Lit(val name: String) extends Atom
case class Neg(val l: Lit) extends Atom
