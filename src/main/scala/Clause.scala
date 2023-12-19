import stainless.collection.{List => List}
import stainless.collection.List.*

/**
 * A clause is a disjunction of literals.
 */
case class Clause(val lits: List[Literal]) {
  override def toString(): String = {
    mkString(lits, " V ", (l: Literal) => l.toString)
  }

  /**
   * Returns a clause that is the result of removing the given literal from
   */
  def rm(lit: Literal): Clause = {
    Clause(lits.filter(_ != lit))
  }.ensuring(c => 
    c.lits.size <= lits.size &&
    c.lits.forall(_ != lit)
  )

}
