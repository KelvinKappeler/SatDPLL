import stainless.collection.{List => List}
import stainless.collection.List.{mkString => lstToString}

class Clause(val l: List[Literal]) {
  override def toString(): String = {
    def litToString(l: Literal) = l.toString
    lstToString(this.l, " V ", litToString)
  }


  def rm(lit: Literal): Clause = {
    Clause(this.l.filter(_ != lit))
  }
  .ensuring(c => 
    c.l.size <= this.l.size &&
    c.l.content.subsetOf(c.l.content) &&
    c.l.forall(_ != lit)  
  )
}