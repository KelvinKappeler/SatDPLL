sealed abstract class Literal {
  def neg: Literal = {
    this match {
      case l@Lit(name) => Neg(l)
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

case class Lit(val name: String) extends Literal
case class Neg(val l: Lit) extends Literal
