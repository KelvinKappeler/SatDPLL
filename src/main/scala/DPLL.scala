import stainless.collection.{List => List}
import stainless.collection.ListOps.FlattenableListOps
import stainless.lang.Option as Option
import stainless.lang.Some as Some
import stainless.lang.None as None
import stainless.lang.{Map => Map}
import stainless.lang.Map.ToMapOps
import stainless.collection.Nil
import stainless.collection.Cons as Cons
import stainless.lang.decreases

object DPLL {
  /** Returns a satisfying assignment for the given list of clauses, if one
    * exists. Otherwise, returns None.
    *
    * @param terms
    *   A list of clauses
    * @return
    *   A satisfying assignment for the given list of clauses, if one exists.
    */
  def solve(f: Formula): Option[List[Literal]] = {
    def dpll(formula: Formula, unassigned: List[Literal], assigned: List[Literal], compteur: BigInt): Option[List[Literal]] = {
      decreases(compteur)
      require(compteur >= 0)
      if (compteur == 0) return None()
      if (formula.clauses.isEmpty) return Some(assigned)
      if (unassigned.isEmpty) return None()

      // unit propagation
      val unit = formula.getUnit
      if (unit.isDefined) {
        val lit = unit.get
        return dpll(formula.rmClause(lit).rm(lit.neg), unassigned.filter(_ != lit.positive), lit :: assigned, compteur - 1)
      }

      // pure literal elimination
      val pure = formula.getPure
      if (pure.isDefined) {
        val lit = pure.get
        return dpll(formula.rmClause(lit).rm(lit.neg), unassigned.filter(_ != lit.positive), lit :: assigned, compteur - 1)
      }

      // Test if an assignment is possible for the first variable
      val asTrue = dpll(formula.rmClause(unassigned.head).rm(unassigned.head.neg), unassigned.tail, unassigned.head :: assigned, compteur - 1)

      // Do the same with the inverse of the first variable
      val asFalse = dpll(formula.rmClause(unassigned.head.neg).rm(unassigned.head), unassigned.tail, unassigned.head.neg :: assigned, compteur - 1)
      if (asFalse.isDefined) return Some(asFalse.get)

      None()
    }

    dpll(f.unique, f.distinct, List(), 10000)
   }

  /** Returns the answer from the given list of literals as a string.
    *
    * @param literals
    *   A list of literals
    * @return
    *   A string from the given list of literals
    */
  def answer(optLits: Option[List[Literal]]): String = {
    def toString(lits: List[Literal]): String = {
      decreases(lits)

      lits match {
        case Nil() => ""
        case Cons(head, tail) => head match {
          case Atom(name) => name + " -> true\n" + toString(tail)
          case Neg(Atom(name)) => name + " -> false\n" + toString(tail)
        }
      }
    }
    optLits match {
      case None() => "UNSAT"
      case Some(lits) => toString(lits)
    }
  }
}
