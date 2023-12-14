import stainless.collection.{List => List}
import stainless.collection.ListOps.FlattenableListOps
import stainless.lang.Option as Option
import stainless.lang.Some as Some
import stainless.lang.None as None
import stainless.lang.{Map => Map}
import stainless.lang.Map.ToMapOps
import stainless.collection.Nil
import stainless.io.StdOut.{println => println, print => print}
import stainless.lang.decreases

object DPLL {
  //implicit val state: stainless.io.State = stainless.io.newState

  /** Returns a satisfying assignment for the given list of clauses, if one
    * exists. Otherwise, returns None.
    *
    * @param terms
    *   A list of clauses
    * @return
    *   A satisfying assignment for the given list of clauses, if one exists.
    */
  def solve(f: Formula): Boolean = {
    //require(f.clauses.nonEmpty && f.clauses.forall(_.atoms.nonEmpty))

    // print("\t\tRemove clauses containing the head: ")
    // println(f.rmClause(f.clauses.head.lits.head))

    // print("\t\tCleaned clauses: ")
    // println(f.cleanClauses)

    // print("\t\tPure literal: ")
    // println(f.getPure)

    // print("\t\tUnit Literal: ")
    // println(f.getUnit)

    val result = dpll(f, f.distinct, List())
    if (result.isDefined) {
      //println(toAssignment(result.get))
      true
    } else false
  }

  /** Resolve the satisfiability of the given clauses using the DPLL algorithm.
    * @param formula A list of clauses
    * @param unassigned; A list of unassigned atoms
    * @param assigned A list of assigned atoms
    * @return A satisfying assignment for the given list of clauses, if one exists.
    */
  def dpll(formula: Formula, unassigned: List[Atom], assigned: List[Atom]): Option[List[Atom]] = {
    decreases(unassigned)

    if (formula.clauses.isEmpty) return Some(assigned)
    if (unassigned.isEmpty) return None()

    // unit propagation
    val unit = formula.getUnit
    if (unit.isDefined) {
      val atom = unit.get
      //unassigned.contains(atom.asLit)
      return dpll(formula.rmClause(atom).rm(atom.neg), unassigned.filter(_ != atom.asLit), atom :: assigned)
    }

    // pure literal elimination
    val pure = formula.getPure
    if (pure.isDefined) {
      val atom = pure.get
      //unassigned.contains(atom.asLit)
      return dpll(formula.rmClause(atom).rm(atom.neg), unassigned.filter(_ != atom.asLit), atom :: assigned)
    }

    // Test if an assignment is possible for the first variable
    val asTrue = dpll(formula.rmClause(unassigned.head).rm(unassigned.head.neg), unassigned.tail, unassigned.head :: assigned)
    if (asTrue.isDefined) return Some(asTrue.get)

    // Do the same with the inverse of the first variable
    val asFalse = dpll(formula.rmClause(unassigned.head.neg).rm(unassigned.head), unassigned.tail, unassigned.head.neg :: assigned)
    if (asFalse.isDefined) return Some(asFalse.get)

    None()
  }

  /** Returns an assignment from the given list of literals.
    *
    * @param literals
    *   A list of literals
    * @return
    *   An assignment from the given list of literals.
    */
  /*def toAssignment(atoms: List[Atom]): Map[String, Boolean] = {
    atoms.map {
      case Lit(name) => (name, true)
      case Neg(Lit(name)) => (name, false)
    }.toMap
  }*/
}
