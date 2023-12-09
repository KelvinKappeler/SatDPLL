import stainless.collection.{List => List}
import stainless.io.StdOut.{println => println, print => print}
import stainless.collection.{List => List}
import stainless.collection.ListOps.FlattenableListOps
import stainless.lang.Option as Option
import stainless.lang.Some as Some
import stainless.lang.None as None
import stainless.lang.{Map => Map}
import stainless.lang.Map.ToMapOps

object Test {

  implicit val state: stainless.io.State = stainless.io.newState
  private val q = Lit("Q")
  private val p = Lit("P")
  private val r = Lit("R")
  private val s = Lit("S")
  private val t = Lit("T")
  private val u = Lit("U")
  private val v = Lit("V")

  def testAll(): Boolean = testSat() && testUnsat()

  def testSat(): Boolean = {
    println("Testing satisfying assignments:")

    val sat1: Formula = List(List(q), List(q, q.neg, p.neg))
    print("\tSolving SAT formula ")
    println(sat1)
    DPLL.solve(sat1)

    val sat2: Formula = List(List(q), List(q.neg, p.neg))
    print("\tSolving SAT formula ")
    println(sat2)
    DPLL.solve(sat2)

    // Satisfying with
    //  q <- false
    //  p <- true
    //  r <- false
    //  s <- true
    //  t <- false
    //  u <- false
    val sat3: Formula = List(
      List(t.neg, s, q.neg),
      List(r.neg, s.neg, p.neg),
      List(s.neg, r.neg, q.neg),
      List(r.neg, u, p.neg),
      List(t.neg, p, r),
      List(q.neg, t, r),
      List(t.neg, r.neg, s),
      List(p.neg, r.neg, s.neg),
      List(p, r.neg, t),
      List(t.neg, r, q),
      List(s, p.neg, q),
      List(s.neg, t.neg, q.neg)
    )
    // DPLL.solve(sat3)
    // print("\tSolving SAT formula ")
    // println(sat3)
    // println()
    true
  }

  def testUnsat(): Boolean = {
    println("Testing unsatisfying assignments")

    // val unsat1: Formula = List(List(q), List(q.neg), List(q.neg, q))
    // print("\tSolving UNSAT formula ")
    // println(unsat1)
    // DPLL.solve(unsat1)

    val unsat2: Formula = List(
      List(t.neg, s, q.neg),
      List(r.neg, s.neg, p.neg),
      List(s.neg, r.neg, q.neg),
      List(r.neg, u, p.neg),
      List(t.neg, p, r),
      List(q.neg, t, r),
      List(t.neg, r.neg, s),
      List(p.neg, r.neg, s.neg),
      List(p, r.neg, t),
      List(t.neg, r, q),
      List(s, p.neg, q),
      List(s.neg, t.neg, q.neg),
      // the following 2 clauses makes the formula unsat
      List(s.neg, v, q),
      List(s.neg, v.neg, q)
    )
    // print("\tSolving UNSAT formula ")
    // println(unsat2)
    true
  }
}
