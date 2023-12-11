import stainless.collection.{List => List}
import stainless.collection.List.{mkString => lstToString}
import stainless.collection.ListOps.FlattenableListOps
import stainless.lang.Option as Option
import stainless.lang.Some as Some
import stainless.lang.None as None
import stainless.collection.Nil

// import stainless.lang.{Map => Map}

class Formula(val clauses: List[Clause]) {
  override def toString(): String = { 
    def cToString(c: Clause) = c.toString()
    // The symbol '∧' cause concurrency bugs in the compiler
    "(" + lstToString(this.clauses, ") /\\ (", cToString) + ")"
  }
  def distinct: List[Literal] = this.flatten.unique

  def flatten: List[Literal] = this.clauses.map(_.lits).flatten

  /** Removes all clauses containing the input literal and removes from all clauses
   * the negated input literal.
   * @param lit
   *  The literal to remove.
   * @return
   *  A new formula not containing any clauses with the input lit and not negated input lit.
  */
  // def rm(lit: Literal): Formula = {
  //   Formula(this.c.map(_.rm(lit)).filter(_.lits.nonEmpty))
  // }.ensuring(res => 
  //   res.c.size <= this.c.size && // verified
  //   res.c.forall(_.lits.nonEmpty) // verified
  //   !res.flatten.contains(lit)
  //   !res.flatten.exists(_ == lit)
  //   res.flatten.content.subsetOf(this.flatten.content) &&
  //   res.c.forall(!_.lits.contains(lit))
  // )

  def rmClause(lit: Literal): Formula = {
    require(this.clauses.nonEmpty 
      && this.clauses.forall(_.lits.nonEmpty) 
      && this.clauses.find(c => c.lits.contains(lit)).isDefined
    )
    val f = Formula(this.clauses.filter(c => !c.lits.contains(lit)))
    f.clauses.forall(c => !c.lits.contains(lit))
    // this.clauses.filter(c => c.lits.contains(lit)).forall(c => !f.clauses.contains(c))
    f
  } ensuring { res => 
    res.clauses.size <= this.clauses.size                 // verified
    && res.clauses.content.subsetOf(this.clauses.content) // verified
    && res.clauses.forall(c => !c.lits.contains(lit))     // verified
    && this.clauses.filter(c => c.lits.contains(lit)).size >= 1
  }
    
  /** Returns a unit clause, if one exists. A unit clause is a clause with only
  * one literal, i.e Lit or NegLit(Lit).
  *

  * @return
  *   A unit clause, if one exists.
  */
  // def getUnit: Option[Literal] = {
  //   require(this.clauses.nonEmpty && this.clauses.forall(_.lits.nonEmpty) && this.clauses.forall(_.toString != ""))
  //   this.clauses.find(_.lits.size == 1).map(_.lits.head)
  // }.ensuring { res =>
  //   if res.isDefined then 
  //     this.clauses.exists(c => c.lits.contains(res.get) && c.lits.size == 1) 
  //   else this.clauses.forall(_.lits.size != 1)
  // } 

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
  // def getPure: Option[Literal] = {
  //   val lits = this.flatten
  //   (lits: List[Literal] @unchecked).find {
  //     case (l: Lit)       => !lits.contains(NegLit(l))
  //     case NegLit(l: Lit) => !lits.contains(l)
  //     case _              => false // Intentional, do not remove
  //   }
  // }.ensuring(res =>
  //   val lits = this.flatten
  //   res match {
  //     case Some(lit) =>
  //       lit match {
  //         case (l: Lit)       => !lits.contains(NegLit(l))
  //         case NegLit(l: Lit) => !lits.contains(l)
  //       }
  //     case None() =>
  //       lits.forall(l => lits.contains(l.neg)) // not verified
  //   }
  // )



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
  // def cleanClauses: Formula = {
  //   val filtered = this.clauses map { c => Clause(c.lits filter { l => !c.lits.contains(l.neg) }) }
  //   Formula(filtered.map(c => Clause(c.lits.unique)).filterNot(_.lits.isEmpty))
  // }.ensuring { res =>
  //   res.clauses.forall { c => !c.lits.exists(lit => c.lits.contains(lit.neg)) } &&
  //   res.clauses.forall(_.lits.nonEmpty)
  // }
}