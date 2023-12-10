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
  def solve(f: Formula): Option[Map[String, Boolean]] = {
    require(f.nonEmpty && f.head.nonEmpty)
    print("A unit clause of ")
    print(f)
    print(" is: ")
    println(getUnitCl(f))
    // val result = dpll(distinct(f), cleanClauses(f), List())
    // if (result.isDefined)
    //   Some[Map[String, Boolean]](toAssignment(result.get))
    // else None[Map[String, Boolean]]()
    None[Map[String, Boolean]]()
  }//.ensuring(res => res.isInstanceOf[None[Map[String, Boolean]]])

  /** Resolve the satisfiability of the given clauses using the DPLL algorithm.
    *
    * @param unassigned
    *   A list of unassigned variables
    * @param clauses
    *   A list of clauses
    * @param assignment
    *   A list of assigned literals
    * @return
    *   A satisfying assignment for the given list of clauses, if one exists.
    */
  private def dpll(
      unas: List[Literal],
      f: Formula,
      as: List[Literal]
  ): Option[List[Literal]] = {
    if (f.isEmpty) return Some[List[Literal]](as)
    // if (unas.isEmpty) return None[List[Literal]]()

    // Unit Clause Rule
    // val unitClause = getUnitCl(f)
    // if (unitClause.isDefined) {
    //   val term = unitClause.get
    //   return dpll(
    //     unas,
    //     remove(f, term),
    //     term :: as
    //   )
    // }

    // Pure Literal Rule
    // val pureLiteral = getPureLit(f)
    // if (pureLiteral.isDefined) {
    //   val term = pureLiteral.get
    //   return dpll(
    //     unas,
    //     remove(f, term),
    //     term :: as
    //   )
    // }

    // Test if an assignment is possible for the first variable
    // val tryFirstVarAssignment = dpll(
    //   unas.tail,
    //   remove(f, unas.head),
    //   unas.head :: as
    // )
    // if (tryFirstVarAssignment.isDefined) return tryFirstVarAssignment

    // Do the same with the inverse of the first variable
    // val tryFirstVarInverseAssignment = dpll(
    //   unas.tail,
    //   remove(f, unas.head.neg),
    //   unas.head.neg :: as
    // )
    // if (tryFirstVarInverseAssignment.isDefined)
    //   return tryFirstVarInverseAssignment

    // If neither is possible
    return None[List[Literal]]()
  }

  /** Remove all clauses that contain the given literal and remove the inverse
    * of the given literal from all clauses.
    *
    * @param clauses
    *   A list of clauses
    * @param literal
    *   A literal
    * @return
    *   A list of clauses with no clauses that contain the given literal and no
    *   inverse of the given literal.
    */
  private def rmlit(f: Formula, lit: Literal): Formula = {
    f
      .map(_.filter(_ != lit.neg))
      // Above code filters out the negation of lit from all clauses
      .filterNot(c => c.contains(lit))
      // Above code filters out clauses containing @lit
      .filter(_.nonEmpty)
  }.ensuring(res =>
    res.size <= f.size &&
      !res.flatten.contains(lit) &&
      !res.exists(_.contains(lit)) &&
      res.forall(_.nonEmpty)
  )

  /** Returns a unit clause, if one exists. A unit clause is a clause with only
    * one literal, i.e Lit or NegLit(Lit).
    *
    * @param f
    *   A list of clauses
    * @return
    *   A unit clause, if one exists.
    */
  private def getUnitCl(f: Formula): Option[Literal] = {
    require(f.nonEmpty && f.head.nonEmpty && f.forall(!_.exists(_ == Nil())))
    f.find(_.size == 1).map(_.head)
  }.ensuring { res =>
    if res.isDefined then 
      f.exists(c => c.contains(res.get) && c.size == 1) 
    else f.forall(_.size != 1)
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
  private def getPureLit(f: Formula): Option[Literal] = {
    require(f != Nil() && f.nonEmpty && f.head.nonEmpty && f.forall(_.nonEmpty))
    val lits = f.flatten
    (lits: List[Literal] @unchecked).find {
      case (l: Lit)       => !lits.contains(NegLit(l))
      case NegLit(l: Lit) => !lits.contains(l)
      case _              => false // Intentional, do not remove
    }
  }.ensuring(res =>
    val lits = f.flatten
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

  /** Returns an assignment from the given list of literals.
    *
    * @param literals
    *   A list of literals
    * @return
    *   An assignment from the given list of literals.
    */
  private def toAssignment(lits: List[Literal]): Map[String, Boolean] = {
    lits.map {
      case Lit(name)         => (name, true)
      case NegLit(Lit(name)) => (name, false)
    }.toMap
  }

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
  // private def cleanClauses(f: Formula): Formula = {
  //   val filtered = f map { c => c filter { l => !c.contains(l.neg) } }
  //   filtered.map(_.unique).filterNot(_.isEmpty)
  // }.ensuring { res =>
  //   res.forall { c => !c.exists(lit => c.contains(lit.neg)) } &&
  //   res.forall(_.nonEmpty)
  // }
}
