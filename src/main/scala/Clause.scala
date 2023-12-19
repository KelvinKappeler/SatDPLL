import stainless.collection.{List => List}
import stainless.collection.{Cons => Cons}
import stainless.collection.List.*
import stainless.collection.Nil
import stainless.lang.decreases

/**
 * A clause is a disjunction of literals.
 */
case class Clause(val lits: List[Literal]) {
  override def toString(): String = {
    mkString(lits, " V ", (l: Literal) => l.toString)
  }

  def size: BigInt = this.lits.size ensuring (_ == this.lits.size)

  /**
   * Returns a clause that is the result of removing the given literal from
   */
  def rm(lit: Literal): Clause = {
    this.filterNotLit(lit)
  }.ensuring(res => 
    res.lits.size <= lits.size
    && !res.contains(lit)
    && res.lits.forall(_ != lit)
  )

  def contains(lit: Literal): Boolean = {
    this.lits.contains(lit) 
  } ensuring { 
    _ == (this.lits contains lit)
  }

  def filterNotLit(lit: Literal): Clause = {
    decreases(this.size) 
    this.lits match {
    case Nil() => Clause(List())
    case Cons(h, t) if h != lit =>
      Clause(Cons(h, Clause(t).filterNotLit(lit).lits))
    case Cons(_, t) => Clause(t).filterNotLit(lit)
  }} ensuring { res =>
    res.size <= this.size
    && !res.lits.contains(lit)
    && res.lits.forall(_ != lit)
  }
}
