sealed abstract class Term

case class Var(name: String) extends Term
case class And(left: Term, right: Term) extends Term
case class Or(left: Term, right: Term) extends Term
case class Not(term: Term) extends Term
case class Implies(left: Term, right: Term) extends Term
case class Iff(left: Term, right: Term) extends Term

sealed abstract class Literal {
  def inverse: Literal = this match {
    case VarLiteral(name) => NotLiteral(VarLiteral(name))
    case NotLiteral(VarLiteral(name)) => VarLiteral(name)
  }
}
case class VarLiteral(name: String) extends Literal
case class NotLiteral(variable: VarLiteral) extends Literal
