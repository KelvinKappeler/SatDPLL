import stainless.collection.{List => List}
import stainless.collection.List.*

/**
 * A clause is a disjunction of atoms.
 */
case class Clause(val atoms: List[Atom]) {
  override def toString(): String = {
    mkString(atoms, " V ", (l: Atom) => l.toString)
  }

  /**
   * Returns a clause that is the result of removing the given atom from
   */
  def rm(atom: Atom): Clause = {
    Clause(atoms.filter(_ != atom))
  }.ensuring(c => 
    c.atoms.size <= atoms.size &&
    c.atoms.forall(_ != atom)
  )

}
