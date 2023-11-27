object Transformation {

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

  def toCNF(term: Term): Term = toNNF(term) match {
    case Or(left, And(r1, r2)) => And(Or(left, r1), Or(left, r2))
    case Or(And(l1, l2), right) => And(Or(l1, right), Or(l2, right))
    case And(left, right) => And(toCNF(left), toCNF(right))
    case Or(left, right) => Or(toCNF(left), toCNF(right))
    case _ => term
  }

}
