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
  def distinct: List[Literal] = flatten.map(_.positive).unique

  /**
    * Returns a list of all literals in the formula.
    * @return a list of all literals in the formula.
    */
  def flatten: List[Literal] = clauses.map(_.lits).flatten

  def unique: Formula = {
    decreases(clauses.size)
    clauses match {
      case Nil() => Formula(List())
        case Cons(h, t) =>
        Formula(Cons(h, Formula(t - h).unique.clauses))
    }
  }

  def assign(lit: Literal): Formula = {
    require(clauses.nonEmpty)
    require(clauses == distinct)
    rmClause(lit).rm(lit.neg)
  }

  private def rm(lit: Literal): Formula = { 
    decreases(this.clauses.size)
    this.clauses match {
      case Nil() => Formula(List())
      case Cons(h, t) => {
        val filtered = h.filterNotLit(lit)
        if filtered.lits.isEmpty then Formula(t).rm(lit)
        else Formula(Cons(filtered, Formula(t).rm(lit).clauses))
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
    require(clauses == distinct)
    decreases(clauses.size)
    clauses match {
      case Nil() => Formula(List())
      case Cons(h, Nil()) => {
        if h.contains(lit) then Formula(List(h))
        else Formula(List())
      }
      case Cons(h, t) if h.contains(lit) => Formula(t).rmClause(lit)
      case Cons(h, t) => Formula(Cons(h, Formula(t).rmClause(lit).clauses))
    }
    // Formula(clauses.filter(c => !c.contains(lit)))
  } ensuring { res => 
    res.clauses.size <= clauses.size
    // need the following postcondition to express that the result has clauses
    // that previously contained lit that is not there anymore
    && (if this.clauses.head.contains(lit) then res.clauses.head != this.clauses.head else true)
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

  private def forall(p: Clause => Boolean): Boolean = this.clauses match {
    case Nil() => true
    case Cons(h, t) => p(h) && t.forall(p)
  }
  
}
