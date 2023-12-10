import stainless.collection.{List => List}
import stainless.collection.ListOps.FlattenableListOps
import stainless.lang.Option as Option
import stainless.lang.Some as Some
import stainless.lang.None as None
import stainless.lang.{Map => Map}
import stainless.lang.Map.ToMapOps
import stainless.collection.Nil
import stainless.io.StdOut.{println => println, print => print}

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
    require(f.c.nonEmpty && f.c.head.l.nonEmpty)
    // val result = dpll(f.distinct, f.cleanClauses, List())
    // if (result.isDefined)
    //   Some[Map[String, Boolean]](toAssignment(result.get))
    // else None[Map[String, Boolean]]()
    false
  }//.ensuring(res => res.isInstanceOf[None[Map[String, Boolean]]])

  /** Resolve the satisfiability of the given clauses using the DPLL algorithm.
    *
    * @param unas
    *   A list of unassigned variables
    * @param f
    *   A list of clauses
    * @param as
    *   A list of assigned literals
    * @return
    *   A satisfying assignment for the given list of clauses, if one exists.
    */
  private def dpll(unas: List[Literal], f: Formula, as: List[Literal]): Boolean = {
    if (f.c.isEmpty) return true
    if (unas.isEmpty) return false

    // // unit propagation
    // val unit = f.getUnit
    // if (unit.isDefined) {
    //   val term = unit.get
    //   return dpll(unas, f.rm(term), term :: as)
    // }

    // // pure literal elimination
    // val pure = f.getPure
    // if (pure.isDefined) {
    //   val term = pure.get
    //   return dpll(unas, f.rm(term), term :: as)
    // }

    // Test if an assignment is possible for the first variable
    val asTrue = dpll(unas.tail, f.rm(unas.head), unas.head :: as)
    if asTrue then return true

    // Do the same with the inverse of the first variable
    val asNeg = dpll(unas.tail, f.rm(unas.head.neg), unas.head.neg :: as)
    if asNeg then return false

    // If neither is possible
    return false
  }

  /** Returns an assignment from the given list of literals.
    *
    * @param literals
    *   A list of literals
    * @return
    *   An assignment from the given list of literals.
    */
//   private def toAssignment(lits: List[Literal]): Map[String, Boolean] = {
//     lits.map {
//       case Lit(name)         => (name, true)
//       case NegLit(Lit(name)) => (name, false)
//     }.toMap
//   }
}
