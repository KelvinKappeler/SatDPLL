import stainless.collection.{List => List}
import stainless.collection.List.*
import stainless.io.StdOut.{println => println, print => print}
import stainless.collection.ListOps.FlattenableListOps
import stainless.collection.{Nil => Nil}

sealed abstract class Literal {
  implicit val state: stainless.io.State = stainless.io.newState

  @unchecked
  def neg: Literal = (this: Literal @unchecked) match {
    case Lit(name) => Neg(this.asInstanceOf[Lit])
    case Neg(l) => l
    case _         => Lit("")
  }

  def cmp(o: Literal): Boolean = (this match {
    case Lit(name) => o.isInstanceOf[Lit] && name == o.asInstanceOf[Lit].name
    case Neg(l) =>
      o.isInstanceOf[Neg] && l.name == o.asInstanceOf[Neg].l.name
  }).ensuring(res => if res then (this == o || o == this) else (this != o && o != this))

  override def toString: String = (this: Literal @unchecked) match {
    case Lit(name)      => name
    case Neg(l: Lit) => "¬" + l.name
    case _              => ""
  }
}
case class Lit(val name: String) extends Literal
case class Neg(val l: Lit) extends Literal

