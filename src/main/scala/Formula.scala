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
    * Returns the set of all positive distinct atoms in the formula.
    * @return the set of all positive distinct atoms in the formula.
    */
  def distinct: List[Atom] = flatten.map(atom => atom.asLit).unique

  /**
    * Returns a list of all atoms in the formula.
    * @return a list of all atoms in the formula.
    */
  def flatten: List[Atom] = clauses.map(_.atoms).flatten

  /**
   * Removes atom from each clause in the formula.
   * @param atom The atom to remove.
   * @return A new formula not containing the given atom.
  */
  def rm(atom: Atom): Formula = {
    Formula(clauses.map(_.rm(atom)).filter(_.atoms.nonEmpty))
    }.ensuring(res => 
      res.clauses.size <= clauses.size &&
      res.clauses.forall(_.atoms.nonEmpty)
      //   !res.flatten.contains(lit)
      //   !res.flatten.exists(_ == lit)
      //   res.flatten.content.subsetOf(this.flatten.content) &&
      //  res.c.forall(!_.lits.contains(lit))
   )

  /**
    * Returns a formula that is the result of removing clauses that contain the given atom.
    * @param atom The atom to remove.
    * @return A new formula not containing clauses that contain the given atom.
    */
  def rmClause(atom: Atom): Formula = {
    require(clauses.nonEmpty 
      /*&& clauses.forall(_.atoms.nonEmpty)*/
      /*&& clauses.find(c => c.atoms.contains(atom)).isDefined*/
    )
    Formula(clauses.filter(c => !c.atoms.contains(atom)))
  } ensuring { res => 
    res.clauses.size <= clauses.size
    && res.clauses.forall(c => !c.atoms.contains(atom))
    //&& clauses.filter(c => c.atoms.contains(atom)).size >= 1
  }
    
  /** Returns a unit clause, if one exists. A unit clause is a clause with only
  * one atom, i.e Lit or Neg(Lit).
  *
  * @return
  *   A unit clause, if one exists.
  */
  def getUnit: Option[Atom] = {
    require(clauses.nonEmpty) // && clauses.forall(_.atoms.nonEmpty))
    val clause = clauses.find(_.atoms.size == 1)
    if (clause.isDefined) {
      Some(clause.get.atoms.head)
    } else {
      None()
    }
  }/*.ensuring { res =>
    if (res.isDefined) 
    clauses.exists(c => c.atoms.contains(res.get) && c.atoms.size == 1) 
    else clauses.forall(_.atoms.size != 1)
  }*/

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
  def getPure: Option[Atom] = {
    val atoms = flatten
    atoms.find(atom => atom match
      case l@Lit(_) => !atoms.contains(Neg(l))
      case Neg(l) => !atoms.contains(l)
    )
   }/*.ensuring(res =>
     val atoms = flatten
     res match {
       case Some(atom) =>
         atom match {
           case l@Lit(_) => !atoms.contains(Neg(l))
           case Neg(l: Lit) => !atoms.contains(l)
         }
       case None() =>
         atoms.forall(l => atoms.contains(l.neg)) // not verified
     }
   )*/



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