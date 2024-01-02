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
    def dpll(formula: Formula, unassigned: List[Literal], assigned: List[Literal]): Option[List[Literal]] = {
      decreases(unassigned.size)
      require(assigned.forall(lit => f.flatten.contains(lit)))
      require(f.distinctLits.nonEmpty)

      def removeLit(lit: Literal): List[Literal] = {
        require(!unassigned.isEmpty && unassigned.contains(lit))
        def putAtEndOfList(lit: Literal, lits: List[Literal]): List[Literal] = {
          decreases(lits)
          lits match {
            case Nil() => Nil()
            case Cons(head, tail) => if (head == lit) lits.reverse else Cons(head, putAtEndOfList(lit, tail))
          }
        }.ensuring(res => res.size == lits.size)

        putAtEndOfList(lit, unassigned).reverse.tail
      }.ensuring(res => res.size < unassigned.size)

      if (/*formula.clauses.isEmpty && */f.eval(assigned)) return Some(assigned)
      if (formula.clauses.isEmpty || unassigned.isEmpty) return None()

      // unit propagation
      val unit = formula.getUnit
      if (unit.isDefined) {
        val lit = unit.get.lits.head
        if (unassigned.contains(lit.positive) && f.flatten.contains(lit)) {
          val newUnassigned = removeLit(lit.positive)
          return dpll(formula.assign(lit), newUnassigned, lit :: assigned)
        }
      }

      // pure literal elimination
      val pure = formula.getPure
      if (pure.isDefined) {
        val lit = pure.get
        if (unassigned.contains(lit.positive) && f.flatten.contains(lit)) {
          val newUnassigned = removeLit(lit.positive)
          return dpll(formula.assign(lit), newUnassigned, lit :: assigned)
        }
      }

      // Test if an assignment is possible for the first variable
      val asTrue = 
        if (f.flatten.contains(unassigned.head)) {
          dpll(formula.assign(unassigned.head), unassigned.tail, unassigned.head :: assigned)
        } else {
          None()
        }
      if (asTrue.isDefined) return Some(asTrue.get)
      
      // Do the same with the inverse of the first variable
      val asFalse = 
        if (f.flatten.contains(unassigned.head.neg)) {
          dpll(formula.assign(unassigned.head.neg), unassigned.tail, unassigned.head.neg :: assigned)
        } else {
          None()
        }
      if (asFalse.isDefined) return Some(asFalse.get)

      None()
    } ensuring { (res: Option[List[Literal]]) => res match {
      case Some(as) => f.eval(as)
      case None() => !f.eval(assigned)
    }}
    val nonEmptyClauses = Formula(f.clauses.filter(_.lits.nonEmpty))
    if (f.distinctLits.isEmpty) 
      return Some(List())
    dpll(nonEmptyClauses, nonEmptyClauses.distinctLits, List())
  } ensuring { res => res match {
    case Some(as) if as.nonEmpty => f.eval(as)
    case _ => true
  }}

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
