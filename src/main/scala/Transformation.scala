import stainless.collection.{List => List}
import stainless.annotation.*
import stainless.lang.decreases

sealed abstract class Term {

  /** Returns the negation normal form of the given term.
  *   @return The negation normal form of the given term.
  */
  def toNNF: Term = {
    decreases(this)

    this match {
      case Not(Not(term)) => term.toNNF
      case Not(And(left, right)) => Or(Not(left).toNNF, Not(right).toNNF)
      case Not(Or(left, right)) => And(Not(left).toNNF, Not(right).toNNF)
      case Not(Implies(left, right)) => And(left.toNNF, Not(right).toNNF)
      case Not(Iff(left, right)) => Or(And(left.toNNF, Not(right).toNNF), And(Not(left).toNNF, right.toNNF))
      case And(left, right) => And(left.toNNF, right.toNNF)
      case Or(left, right) => Or(left.toNNF, right.toNNF)
      case Implies(left, right) => Or(Not(left).toNNF, right.toNNF)
      case Iff(left, right) => Or(And(left.toNNF, right.toNNF), And(Not(left).toNNF, Not(right).toNNF))
      case _ => this
    }
  }.ensuring(res => res.isNNF)

    /** Returns the conjunctive normal form of the given term.
  *
  * @param term
  *   A term
  * @return
  *   The conjunctive normal form of the given term.
  */
  def toCNF: Term = {
    decreases(this)
    require(isNNF)

    this match {
      case Or(left, And(r1, r2))  => And(Or(left, r1), Or(left, r2))
      case Or(And(l1, l2), right) => And(Or(l1, right), Or(l2, right))
      case And(left, right) => And(left.toCNF, right.toCNF)
      case Or(left, right) => Or(left.toCNF, right.toCNF)
      case _ => this
    }
  }

    /** Returns a formula from the given term.
  *
  * @param term
  *   A term
  * @return
  *   A formula from the given term.
  */
  @extern def toFormula: Formula = {
    def rec(term: Term): Clause = {
      term match {
        case Or(left, right) => Clause(rec(left).lits ++ rec(right).lits)
        case Var(name)       => Clause(List(Atom(name)))
        case Not(Var(name))  => Clause(List(Neg(Atom(name))))
        case _               => Clause(List())
      }
    }

    toNNF.toCNF match {
      case Var(name)        => Formula(List(Clause(List(Atom(name)))))
      case Not(Var(name))   => Formula(List(Clause(List(Neg(Atom(name))))))
      case And(left, right) => Formula(left.toFormula.clauses ++ right.toFormula.clauses)
      case Or(left, right)  => Formula(List(Clause(rec(left).lits ++ rec(right).lits)))
      case _                => Formula(List())
    }
  }

  def isNNF: Boolean = this match {
    case Var(_) => true
    case And(l, r) => l.isNNF && r.isNNF
    case Or(l, r) => l.isNNF && r.isNNF
    case Implies(_, _) => false
    case Iff(_, _) => false
    case Not(Var(_)) => true
    case Not(_) => false
  }

}

case class Var(name: String) extends Term { require(name != "") }
case class And(left: Term, right: Term) extends Term
case class Or(left: Term, right: Term) extends Term
case class Not(term: Term) extends Term
case class Implies(left: Term, right: Term) extends Term
case class Iff(left: Term, right: Term) extends Term
