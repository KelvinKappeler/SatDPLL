import stainless.collection.{List => List}
import stainless.collection.List.*
import stainless.io.StdOut.{println => println, print => print}
import stainless.collection.ListOps.FlattenableListOps
import stainless.collection.{Nil => Nil}

sealed abstract class Literal {
  implicit val state: stainless.io.State = stainless.io.newState

  @unchecked
  def neg: Literal = (this: Literal @unchecked) match {
    case Lit(name) => NegLit(this.asInstanceOf[Lit])
    case NegLit(l) => l
    case _         => Lit("")
  }

  def cmp(o: Literal): Boolean = (this match {
    case Lit(name) => o.isInstanceOf[Lit] && name == o.asInstanceOf[Lit].name
    case NegLit(l) =>
      o.isInstanceOf[NegLit] && l.name == o.asInstanceOf[NegLit].l.name
  }).ensuring(res => if res then (this == o || o == this) else (this != o && o != this))

  override def toString: String = (this: Literal @unchecked) match {
    case Lit(name)      => name
    case NegLit(l: Lit) => "¬" + l.name
    case _              => ""
  }
}
case class Lit(val name: String) extends Literal
case class NegLit(val l: Lit) extends Literal

