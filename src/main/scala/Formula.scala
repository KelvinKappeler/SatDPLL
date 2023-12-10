import stainless.collection.{List => List}
import stainless.collection.List.{mkString => lstToString}
import stainless.collection.ListOps.FlattenableListOps
import stainless.lang.Option as Option
import stainless.lang.Some as Some
import stainless.lang.None as None
// import stainless.lang.{Map => Map}

class Formula(val c: List[Clause]) {
  override def toString(): String = { 
    def cToString(c: Clause) = c.toString()
    // The symbol '∧' cause concurrency bugs in the compiler
    "(" + lstToString(this.c, ") ∧ (", cToString) + ")"
  }
  def distinct: List[Literal] = this.flatten.unique
  def flatten: List[Literal] = this.c.map(_.l).flatten

  def rm(lit: Literal): Formula = {
    Formula(this.c.map(_.rm(lit)).filter(_.l.nonEmpty))
  }.ensuring(res => 
    res.c.size <= this.c.size && // verified
    res.c.forall(_.l.nonEmpty) // verified
    !res.flatten.contains(lit)
    !res.flatten.exists(_ == lit)
    res.flatten.content.subsetOf(this.flatten.content) &&
    res.c.forall(!_.l.contains(lit))
  )

  /** Returns a unit clause, if one exists. A unit clause is a clause with only
  * one literal, i.e Lit or NegLit(Lit).
  *
  * @param f
  *   A list of clauses
  * @return
  *   A unit clause, if one exists.
  */
  def getUnit: Option[Literal] = {
    // require(this.c.nonEmpty && this.c.head.l.nonEmpty && this.c.forall(!_.l.exists(_ == Nil())))
    this.c.find(_.l.size == 1).map(_.l.head)
  }.ensuring { res =>
    if res.isDefined then 
      this.c.exists(_.l.contains(res.get) && c.size == 1) 
    else this.c.forall(_.l.size != 1)
  } 

  /** Returns a pure literal, if one exists. A pure literal is a literal that
  * only appears with one form in the entire formula. For example, in the
  * formula (P ∨ Q) ∧ (¬P ∨ Q), the literal P is not pure because it appears
  * as P and ¬P.
  *
  * @param f
  *   A list of clauses
  * @return
  *   A pure literal, if one exists.
  */
  def getPure: Option[Literal] = {
    val lits = this.flatten
    (lits: List[Literal] @unchecked).find {
      case (l: Lit)       => !lits.contains(NegLit(l))
      case NegLit(l: Lit) => !lits.contains(l)
      case _              => false // Intentional, do not remove
    }
  }.ensuring(res =>
    val lits = this.flatten
    res match {
      case Some(lit) =>
        lit match {
          case (l: Lit)       => !lits.contains(NegLit(l))
          case NegLit(l: Lit) => !lits.contains(l)
        }
      case None() =>
        lits.forall(l => lits.contains(l.neg)) // not verified
    }
  )

  private def rmClauseWithLit(lit: Literal): Formula = {
    Formula(this.c.map(c => c.rm(lit)).filter(!_.l.contains(lit)).filter(_.l.nonEmpty))
  }.ensuring(res => 
    res.c.size <= this.c.size && // verified
    res.c.forall(_.l.nonEmpty) && // verified  
    this.c.forall(_.l.forall(_ != lit))
  )

  /** Filters the given list of clauses by removing all clauses that contain
  * some literal and its inverse and remove all duplicate literals from each
  * clause.
  *
  * @param clauses
  *   A list of clauses
  * @return
  *   A list of clauses with no duplicate literals and no clauses that contain
  *   some literal and its inverse.
  * 
  * Commented out because it is only a "nice-to-have" function, and not necessary.
  */
  def cleanClauses: Formula = {
    val filtered = this.c map { c => Clause(c.l filter { l => !c.l.contains(l.neg) }) }
    Formula(filtered.map(c => Clause(c.l.unique)).filterNot(_.l.isEmpty))
  }.ensuring { res =>
    res.c.forall { c => !c.l.exists(lit => c.l.contains(lit.neg)) } &&
    res.c.forall(_.l.nonEmpty)
  }
}