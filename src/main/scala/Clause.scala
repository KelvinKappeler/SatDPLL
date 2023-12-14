import stainless.collection.{List => List}
import stainless.collection.List.{mkString => lstToString}

case class Clause(val lits: List[Literal]) {
  override def toString(): String = {
    def litToString(l: Literal) = l.toString
    lstToString(this.lits, " V ", litToString)
  }

  def rmLit(lit: Literal): Clause = {
    Clause(this.lits.filter(_ != lit))
  }.ensuring(c => 
    c.lits.size <= this.lits.size &&
    c.lits.content.subsetOf(c.lits.content) &&
    c.lits.forall(_ != lit)  
  )
}