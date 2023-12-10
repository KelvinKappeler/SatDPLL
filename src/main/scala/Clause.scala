import stainless.collection.{List => List}
import stainless.collection.List.{mkString => lstToString}

class Clause(val l: List[Literal]) {
  override def toString(): String = {
    lstToString(this.l, " V ", litToString)
  }
  private def litToString(l: Literal) = l.toString
  def rm(lit: Literal): Clause = {
    Clause(this.l.filter(_ != lit))
  }
  .ensuring(c => 
    c.l.size <= this.l.size &&
    c.l.content.subsetOf(c.l.content) &&
    c.l.forall(_ != lit)  
  )
}