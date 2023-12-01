object Transformation {

  /**
    * Returns the negation normal form of the given term.
    *
    * @param term A term
    * @return The negation normal form of the given term.
    */
  def toNNF(term: Term): Term = term match {
    case Not(Not(term)) => toNNF(term)
    case Not(And(left, right)) => Or(toNNF(Not(left)), toNNF(Not(right)))
    case Not(Or(left, right)) => And(toNNF(Not(left)), toNNF(Not(right)))
    case Not(Implies(left, right)) => And(toNNF(left), toNNF(Not(right)))
    case Not(Iff(left, right)) => Or(And(toNNF(left), toNNF(Not(right))), And(toNNF(Not(left)), toNNF(right)))
    case And(left, right) => And(toNNF(left), toNNF(right))
    case Or(left, right) => Or(toNNF(left), toNNF(right))
    case Implies(left, right) => Or(toNNF(Not(left)), toNNF(right))
    case Iff(left, right) => Or(And(toNNF(left), toNNF(right)), And(toNNF(Not(left)), toNNF(Not(right))))
    case _ => term
  }

  /**
    * Returns the conjunctive normal form of the given term.
    *
    * @param term A term
    * @return The conjunctive normal form of the given term.
    */
  def toCNF(term: Term): Term = toNNF(term) match {
    case Or(left, And(r1, r2)) => And(Or(left, r1), Or(left, r2))
    case Or(And(l1, l2), right) => And(Or(l1, right), Or(l2, right))
    case And(left, right) => And(toCNF(left), toCNF(right))
    case Or(left, right) => Or(toCNF(left), toCNF(right))
    case Implies(left, right) => throw new IllegalArgumentException("Term must be in NNF")
    case Iff(left, right) => throw new IllegalArgumentException("Term must be in NNF")
    case _ => term
  }

  /**
    * Returns a list of clauses from the given term.
    *
    * @param term A term
    * @return A list of clauses from the given term.
    */
  def toClauses(term: Term): List[DPLL.Clause] = {
    def rec(term: Term): DPLL.Clause = {
      term match {
        case Or(left, right) => rec(left) ++ rec(right)
        case Var(name) => List(VarLiteral(name))
        case Not(Var(name)) => List(NotLiteral(VarLiteral(name)))
        case _ => throw new IllegalArgumentException("Term must be in CNF")
      }
    }

    toCNF(term) match {
      case Var(name) => List(List(VarLiteral(name)))
      case Not(Var(name)) => List(List(NotLiteral(VarLiteral(name))))
      case And(left, right) => toClauses(left) ++ toClauses(right)
      case Or(left, right) => List(rec(left) ++ rec(right))
      case _ => throw new IllegalArgumentException("Term must be in CNF")
    }
  }
}
