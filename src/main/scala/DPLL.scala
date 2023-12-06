import stainless.collection.List
import stainless.collection.ListOps.FlattenableListOps
import stainless.lang.Option.*

object DPLL {

  /**
    * Returns a satisfying assignment for the given list of clauses, if one exists.
    * Otherwise, returns None.
    *
    * @param terms A list of clauses
    * @return A satisfying assignment for the given list of clauses, if one exists.
    */
  def solve(terms: List[List[Literal]]): stainless.lang.Option[Map[String, Boolean]] = {
    val result = dpll(getVarNames(terms), cleanClauses(terms), List())
    if (result.isDefined) stainless.lang.Some[Map[String, Boolean]](toAssignment(result.get))
    else stainless.lang.None[Map[String, Boolean]]()
  }

  /**
    * Returns the distinct name of all variables in the given list of clauses.
    *
    * @param clauses A list of clauses
    * @return The distinct name of all variables in the given list of clauses.
    */
  private def getVarNames(clauses: List[List[Literal]]): List[String] = {
    clauses.flatten.map {
      case VarLiteral(name) => name
      case NotLiteral(VarLiteral(name)) => name
    }.distinct
  }

  /**
    * Returns an assignment from the given list of literals.
    *
    * @param literals A list of literals
    * @return An assignment from the given list of literals.
    */
  private def toAssignment(literals: List[Literal]): Map[String, Boolean] = {
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
  private def cleanClauses(clauses: List[List[Literal]]): List[List[Literal]] = {
    val filtered = clauses.filterNot(clause => clause.exists(literal => clause.contains(inverse(literal))))
    filtered.map(_.distinct).filterNot(_.isEmpty)
  }

  /**
    * Resolve the satisfiability of the given clauses using the DPLL algorithm.
    *
    * @param unassignedVar A list of unassigned variables
    * @param clauses A list of clauses
    * @param assignment A list of assigned literals
    * @return A satisfying assignment for the given list of clauses, if one exists.
    */
  private def dpll(unassignedVar: List[String], clauses: List[List[Literal]], assignment: List[Literal]): stainless.lang.Option[List[Literal]] = {
    if (clauses.isEmpty) return stainless.lang.Some[List[Literal]](assignment)
    if (unassignedVar.isEmpty) return stainless.lang.None[List[Literal]]()

    //Unit Clause Rule
    val unitClause = findUnitClause(clauses)
    if (unitClause.isDefined) {
      val term = unitClause.get
      return dpll(unassignedVar, treatNewAssignment(clauses, term), term :: assignment)
    }

    //Pure Literal Rule
    val pureLiteral = findPureLiteral(clauses)
    if (pureLiteral.isDefined) {
      val term = pureLiteral.get
      return dpll(unassignedVar, treatNewAssignment(clauses, term), term :: assignment)
    }

    //Test if an assignment is possible for the first variable
    val first = unassignedVar.head

    val firstVar = VarLiteral(first)
    val tryFirstVarAssignment = dpll(
      unassignedVar.tail,
      treatNewAssignment(clauses, firstVar),
      firstVar :: assignment)
    if (tryFirstVarAssignment.isDefined) return tryFirstVarAssignment

    //Do the same with the inverse of the first variable
    val firstVarInverse = NotLiteral(VarLiteral(first))
    val tryFirstVarInverseAssignment = dpll(
      unassignedVar.tail,
      treatNewAssignment(clauses, firstVarInverse),
      firstVarInverse :: assignment)
    if (tryFirstVarInverseAssignment.isDefined) return tryFirstVarInverseAssignment

    //If neither is possible
    return stainless.lang.None[List[Literal]]()
  }

  /**
    * Remove all clauses that contain the given literal and remove the inverse of the given literal from all clauses.
    * 
    * @param clauses A list of clauses
    * @param literal A literal
    * @return A list of clauses with no clauses that contain the given literal and no inverse of the given literal.
    */
  private def treatNewAssignment(clauses: List[List[Literal]], literal: Literal): List[List[Literal]] = {
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
  private def findUnitClause(clauses: List[List[Literal]]): stainless.lang.Option[Literal] = {
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
  private def findPureLiteral(clauses: List[List[Literal]]): stainless.lang.Option[Literal] = {
    val terms = clauses.flatten
    clauses.flatten.find {
      case VarLiteral(name) => !terms.contains(NotLiteral(VarLiteral(name)))
      case NotLiteral(VarLiteral(name)) => !terms.contains(VarLiteral(name))
    }
  }
}
