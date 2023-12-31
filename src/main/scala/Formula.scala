import stainless.collection.{List => List}
import stainless.collection.{Cons => Cons}
import stainless.collection.List.*
import stainless.collection.ListOps.FlattenableListOps
import stainless.lang.Option as Option
import stainless.lang.Some as Some
import stainless.lang.None as None
import stainless.collection.Nil
import stainless.lang.decreases


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
  def distinctLits: List[Literal] = flatten.map(_.positive).unique

  /**
    * Returns a list of all literals in the formula.
    * @return a list of all literals in the formula.
    */
  def flatten: List[Literal] = clauses.map(_.lits).flatten

  def eval(as: List[Literal]): Boolean = {
    clauses.forall(_.eval(as))
  } ensuring { res =>
    res == clauses.forall(_.eval(as))
  }

  def assign(lit: Literal): Formula = {
    require(clauses.nonEmpty)
    rmClause(lit).rm(lit.neg)
  }

  private def rm(lit: Literal): Formula = { 
    decreases(this.clauses.size)
    this.clauses match {
      case Nil() => Formula(List())
      case Cons(h, t) => {
        val filtered = h.filterNotLit(lit)
        Formula(Cons(filtered, Formula(t).rm(lit).clauses))
      }
    } 
  } ensuring { res => 
    res.clauses.size <= this.clauses.size 
    && res.forall(c => !c.contains(lit))
  }

  /**
    * Returns a formula that is the result of removing clauses that contain the given literal.
    * @param atom The literal to remove.
    * @return A new formula not containing any clauses with the given literal.
    */
  private def rmClause(lit: Literal): Formula = {
    require(clauses.nonEmpty)
    Formula(clauses.filter(!_.contains(lit)))
  } ensuring { res => 
    res.clauses.size <= clauses.size
    && res == Formula(clauses.filter(!_.contains(lit)))
  }

    /** Returns a unit clause, if one exists. A unit clause is a clause with only
  * one literal, i.e Atom or Neg(atom).
  *
  * @return
  *   A unit clause, if one exists.
  */
  def getUnit: Option[Clause] = {
    require(clauses.nonEmpty)
    clauses.find(_.lits.size == 1)
  } ensuring { res => res match {
    case Some(c) => (clauses contains c) && c.lits.size == 1
    case None() => true
  }}

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
  } ensuring { res => 
    val lits = flatten
    res match {
      case Some(atom@Atom(_)) => !lits.contains(Neg(atom))
      case Some(Neg(atom)) => !lits.contains(atom)
      case None() => true
  }}

  private def forall(p: Clause => Boolean): Boolean = this.clauses match {
    case Nil() => true
    case Cons(h, t) => p(h) && t.forall(p)
  }
}
