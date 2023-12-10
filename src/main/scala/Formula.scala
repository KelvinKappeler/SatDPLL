import stainless.collection.{List => List}
import stainless.collection.List.{mkString => lstToString}
import stainless.collection.ListOps.FlattenableListOps
// import stainless.lang.{Map => Map}

class Formula(val c: List[Clause]) {
  override def toString(): String = { // ∧
    "(" + lstToString(this.c, ") ∧ (", cToString) + ")"
  }
  private def cToString(c: Clause) = c.toString()
  def distinct: List[Literal] = this.c.map(_.l).flatten
}