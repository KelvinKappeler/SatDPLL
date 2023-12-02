object DPLL {

  type Clause = List[Literal]
  type Assignment = Map[String, Boolean]

  /**
    * Returns a satisfying assignment for the given list of clauses, if one exists.
    * Otherwise, returns None.
    *
    * @param terms A list of clauses
    * @return A satisfying assignment for the given list of clauses, if one exists.
    */
  def solve(terms: List[Clause]): Option[Assignment] = {
    val cleaned = cleanClauses(terms)
    val result = dpll(getVarNames(terms), cleaned, Nil)
    if (result.isDefined) {
      Some(toAssignment(result.get))
    } else {
      None
    }
  }

  private def getVarNames(clauses: List[Clause]): List[String] = {
    clauses.flatten.map {
      case VarLiteral(name) => name
      case NotLiteral(VarLiteral(name)) => name
    }.distinct
  }

  private def toAssignment(literals: List[Literal]): Assignment = {
    literals.map {
      case VarLiteral(name) => (name, true)
      case NotLiteral(VarLiteral(name)) => (name, false)
    }.toMap
  }

  /**
    * Filters the given list of clauses by removing all clauses that contain some literal and its inverse 
    * and remove all duplicate literals from each clause.
    * 
    * @param clauses A list of clauses
    * @return A list of clauses with no duplicate literals and no clauses that contain some literal and its inverse.
    */
  private def cleanClauses(clauses: List[Clause]): List[Clause] = {
    val filtered = clauses.filterNot(clause => clause.exists(literal => clause.contains(inverse(literal))))
    filtered.map(_.distinct).filterNot(_.isEmpty)
  }

  /**
    * Resolve the satisfiability of the given clauses using the DPLL algorithm.
    *
    * @param unassignedVar
    * @param clauses
    * @param assignment
    * @return
    */
  private def dpll(unassignedVar: List[String], clauses: List[Clause], assignment: List[Literal]): Option[List[Literal]] = {
    if (clauses.isEmpty) {
      return Some(assignment)
    }

    if (unassignedVar.isEmpty) {
      return None
    }

    //Unit Clause
    val unitClause = findUnitClause(clauses)
    if (unitClause.isDefined) {
      val term = unitClause.get
      val newClauses = treatNewAssignment(clauses, term)
      return dpll(unassignedVar, newClauses, term :: assignment)
    }

    //Pure Literal
    val pureLiteral = findPureLiteral(clauses)
    if (pureLiteral.isDefined) {
      val term = pureLiteral.get
      val newClauses = treatNewAssignment(clauses, term)
      return dpll(unassignedVar, newClauses, term :: assignment)
    }

    //test if an assignment is possible for the first variable
    val first = unassignedVar.head

    val firstVar = VarLiteral(first)
    val clearedClauses = treatNewAssignment(clauses, firstVar)
    val tryFirstVarAssignment = dpll(unassignedVar.tail, clearedClauses, firstVar :: assignment)
    if (tryFirstVarAssignment.isDefined) {
      return tryFirstVarAssignment
    }

    //do the same with the inverse of the first variable
    val firstVarInverse = NotLiteral(VarLiteral(first))
    val clearedClauses2 = treatNewAssignment(clauses, firstVarInverse)
    val tryFirstVarInverseAssignment = dpll(unassignedVar.tail, clearedClauses2, firstVarInverse :: assignment)
    if (tryFirstVarInverseAssignment.isDefined) {
      return tryFirstVarInverseAssignment
    }

    //if neither is possible
    return None
  }

  /**
    * Remove all clauses that contain the given literal and remove the inverse of the given literal from all clauses.
    */
  private def treatNewAssignment(clauses: List[Clause], literal: Literal): List[Clause] = {
    val clausesWithoutLiteral = clauses.filterNot(_.contains(literal))
    clausesWithoutLiteral.map(_.filterNot(_ == inverse(literal)))
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
