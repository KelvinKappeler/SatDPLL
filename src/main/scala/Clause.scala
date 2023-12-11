import stainless.collection.{List => List}
import stainless.collection.List.{mkString => lstToString}

class Clause(val lits: List[Literal]) {
  override def toString(): String = {
    def litToString(l: Literal) = l.toString
    lstToString(this.lits, " V ", litToString)
  }

  def eq(o: Clause): Boolean = {
    this.lits.forall(l => o.lits.contains(l))
  }.ensuring(res => res match {
    case true => this.lits.forall(l => o.lits.contains(l))
    case _ => !this.lits.forall(l => o.lits.contains(l))
  })

  def rm(lit: Literal): Clause = {
    Clause(this.lits.filter(_ != lit))
  }
  .ensuring(c => 
    c.lits.size <= this.lits.size &&
    c.lits.content.subsetOf(c.lits.content) &&
    c.lits.forall(_ != lit)  
  )
}