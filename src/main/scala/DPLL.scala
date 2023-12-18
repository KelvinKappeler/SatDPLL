import stainless.collection.{List => List}
import stainless.collection.ListOps.FlattenableListOps
import stainless.lang.Option as Option
import stainless.lang.Some as Some
import stainless.lang.None as None
import stainless.lang.{Map => Map}
import stainless.lang.Map.ToMapOps
import stainless.collection.Nil
import stainless.collection.Cons as Cons
import stainless.io.StdOut.{println => println, print => print}
import stainless.lang.decreases
import stainless.annotation.*


object DPLL {
  implicit val state: stainless.io.State = stainless.io.newState

  /** Returns a satisfying assignment for the given list of clauses, if one
    * exists. Otherwise, returns None.
    *
    * @param terms
    *   A list of clauses
    * @return
    *   A satisfying assignment for the given list of clauses, if one exists.
    */
  def solve(f: Formula): Boolean = {
    def dpll(formula: Formula, unassigned: List[Atom], assigned: List[Atom], compteur: BigInt): Option[List[Atom]] = {
      decreases(compteur)
      require(compteur >= 0)
      if (compteur == 0) return None()
      if (formula.clauses.isEmpty) return Some(assigned)
      if (unassigned.isEmpty) return None()

      // unit propagation
      val unit = formula.getUnit
      if (unit.isDefined) {
        val atom = unit.get
        return dpll(formula.rmClause(atom).rm(atom.neg), unassigned.filter(_ != atom.asLit), atom :: assigned, compteur - 1)
      }

      // pure literal elimination
      val pure = formula.getPure
      if (pure.isDefined) {
        val atom = pure.get
        return dpll(formula.rmClause(atom).rm(atom.neg), unassigned.filter(_ != atom.asLit), atom :: assigned, compteur - 1)
      }

      // Test if an assignment is possible for the first variable
      val asTrue = dpll(formula.rmClause(unassigned.head).rm(unassigned.head.neg), unassigned.tail, unassigned.head :: assigned, compteur - 1)

      // Do the same with the inverse of the first variable
      val asFalse = dpll(formula.rmClause(unassigned.head.neg).rm(unassigned.head), unassigned.tail, unassigned.head.neg :: assigned, compteur - 1)
      if (asFalse.isDefined) return Some(asFalse.get)

      None()
    }

    val result = dpll(f, f.distinct, List(), 10000)
    if (result.isDefined) {
      println(toAssignment(result.get))
      true
    } else {
      println("No solution found")
      false
    }
   }

  /** Returns an assignment from the given list of literals.
    *
    * @param literals
    *   A list of literals
    * @return
    *   An assignment from the given list of literals.
    */
  @ignore def toAssignment(atoms: List[Atom]): String = {
    atoms match {
      case Nil() => ""
      case Cons(head, tail) => head match {
        case Lit(name) => name + " -> true\n" + toAssignment(tail)
        case Neg(Lit(name)) => name + " -> false\n" + toAssignment(tail)
      }
    }
  }
}
