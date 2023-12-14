import stainless.collection.{List => List}
import stainless.collection.List.*
import stainless.io.StdOut.{println => println, print => print}
import stainless.collection.ListOps.FlattenableListOps
import stainless.collection.{Nil => Nil}

sealed abstract class Literal {
  implicit val state: stainless.io.State = stainless.io.newState

  def neg: Literal = this match {
    case l@Lit(name) => Neg(l)
    case Neg(l) => l
  }

  override def toString: String = (this: Literal @unchecked) match {
    case Lit(name)      => name
    case Neg(l: Lit) => "¬" + l.name
    case _              => ""
  }
}
case class Lit(val name: String) extends Literal
case class Neg(val l: Lit) extends Literal

