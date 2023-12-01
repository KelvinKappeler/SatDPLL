object DPLL {

  type Clause = List[Literal]
  type Assignment = Map[Literal, Boolean]

  /**
    * Returns a satisfying assignment for the given list of clauses, if one exists.
    * Otherwise, returns None.
    *
    * @param terms A list of clauses
    * @return A satisfying assignment for the given list of clauses, if one exists.
    */
  def solve(terms: List[Clause]): Option[Assignment] = {
    dpll(terms, Map.empty)
  }

  private def dpll(clauses: List[Clause], assignment: Assignment): Option[Assignment] = {
    if (clauses.isEmpty) return Some(assignment)

    //Unit Clause
    val unitClause = findUnitClause(clauses)
    if (unitClause.isDefined) {
      val term = unitClause.get
      val newClauses = clauses.filterNot(_.contains(term))
      val newAssignment = assignment + (term match {
        case vl@VarLiteral(name) => (vl, true)
        case vl@NotLiteral(VarLiteral(name)) => (vl, false)
      })
      return dpll(newClauses, newAssignment)
    }

    //Pure Literal
    val pureLiteral = findPureLiteral(clauses)
    if (pureLiteral.isDefined) {
      val term = pureLiteral.get
      val newClauses = clauses.filterNot(_.contains(term))
      val newAssignment = assignment + (term match {
        case vl@VarLiteral(name) => (vl, true)
        case vl@NotLiteral(VarLiteral(name)) => (vl, false)
      })
      return dpll(newClauses, newAssignment)
    }

    //TODO: Rest of the algorithm

    return None
  }

  /**
    * Returns a unit clause, if one exists.
    * A unit clause is a clause with only one literal, i.e Var or Not(Var).
    *
    * @param clauses A list of clauses
    * @return A unit clause, if one exists.
    */
  private def findUnitClause(clauses: List[Clause]): Option[Literal] = {
    clauses.find(_.size == 1).map(_.head)
  }

  /**
    * Returns a pure literal, if one exists.
    * A pure literal is a literal that only appears with one form in the entire formula.
    * For example, in the formula (P ∨ Q) ∧ (¬P ∨ Q), the literal P is not pure because it appears as P and ¬P.
    *
    * @param clauses A list of clauses
    * @return A pure literal, if one exists.
    */
  private def findPureLiteral(clauses: List[Clause]): Option[Literal] = {
    val terms = clauses.flatten
    clauses.flatten.find {
      case VarLiteral(name) => !terms.contains(NotLiteral(VarLiteral(name)))
      case NotLiteral(VarLiteral(name)) => !terms.contains(VarLiteral(name))
    }
  }
}
