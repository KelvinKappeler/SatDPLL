import stainless.collection.{List => List}
import stainless.collection.List.*
import stainless.collection.ListOps.FlattenableListOps
import stainless.lang.Option as Option
import stainless.lang.Some as Some
import stainless.lang.None as None
import stainless.collection.Nil

/**
 * A formula is a conjunction of clauses.
 */
case class Formula(val clauses: List[Clause]) {
  override def toString(): String = { 
    mkString(clauses.map(clause => "(" + clause.toString() + ")"), """ /\ """, (str: String) => str)
  }
  
  /**
    * Returns the set of all positive distinct literals in the formula.
    * @return the set of all positive distinct literals in the formula.
    */
  def distinct: List[Literal] = flatten.map(_.positive).unique

  /**
    * Returns a list of all literals in the formula.
    * @return a list of all literals in the formula.
    */
  def flatten: List[Literal] = clauses.map(_.lits).flatten

  /**
   * Removes literal from each clause in the formula.
   * @param atom The literal to remove.
   * @return A new formula not containing the given literal.
  */
  def rm(lit: Literal): Formula = {
    Formula(clauses.map(_.rm(lit)))
    }.ensuring(_.clauses.size <= clauses.size)

  /**
    * Returns a formula that is the result of removing clauses that contain the given literal.
    * @param atom The literal to remove.
    * @return A new formula not containing any clauses with the given literal.
    */
  def rmClause(lit: Literal): Formula = {
    require(clauses.nonEmpty)
    Formula(clauses.filter(c => !c.lits.contains(lit)))
  } ensuring { res => 
    res.clauses.size <= clauses.size
    && res.clauses.forall(c => !c.lits.contains(lit))
  }
    
  /** Returns a unit clause, if one exists. A unit clause is a clause with only
  * one literal, i.e Atom or Neg(atom).
  *
  * @return
  *   A unit clause, if one exists.
  */
  def getUnit: Option[Literal] = {
    require(clauses.nonEmpty)
    val clause = clauses.find(_.lits.size == 1)
    if (clause.isDefined) {
      Some(clause.get.lits.head)
    } else {
      None()
    }
  }

  /**
    * Returns a pure literal, if one exists. A pure literal is a literal that
    * only appears with one form in the entire formula. For example, in the
    * formula (P ∨ Q) ∧ (¬P ∨ Q), the literal P is not pure because it appears
    * as P and ¬P.
    * @param f
    *   A list of clauses
    * @return
    *   A pure literal, if one exists.
    */
  def getPure: Option[Literal] = {
    val lits = flatten
    lits.find(lit => lit match
      case atom@Atom(_) => !lits.contains(Neg(atom))
      case Neg(atom) => !lits.contains(atom)
    )
   }
}
