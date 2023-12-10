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

    val sat1: Formula = Formula(List(Clause(List(q)), Clause(List(q, q.neg, p.neg))))
    print("\tSolving SAT formula ")
    println(sat1)
    DPLL.solve(sat1)

    val sat2: Formula = Formula(List(Clause(List(q)), Clause(List(q.neg, p.neg))))
    print("\tSolving SAT formula ")
    println(sat2)
    // DPLL.solve(sat2)

    // Satisfying with
    //  q <- false
    //  p <- true
    //  r <- false
    //  s <- true
    //  t <- false
    //  u <- false
    val sat3: Formula = Formula(List(
      Clause(List(t.neg, s, q.neg)),
      Clause(List(r.neg, s.neg, p.neg)),
      Clause(List(s.neg, r.neg, q.neg)),
      Clause(List(r.neg, u, p.neg)),
      Clause(List(t.neg, p, r)),
      Clause(List(q.neg, t, r)),
      Clause(List(t.neg, r.neg, s)),
      Clause(List(p.neg, r.neg, s.neg)),
      Clause(List(p, r.neg, t)),
      Clause(List(t.neg, r, q)),
      Clause(List(s, p.neg, q)),
      Clause(List(s.neg, t.neg, q.neg))
    ))
    print("\tSolving SAT formula ")
    println(sat3)
    println()
    // DPLL.solve(sat3)
    true
  }

  def testUnsat(): Boolean = {
    println("Testing unsatisfying assignments")

    val unsat1: Formula = Formula(List(Clause(List(q)), Clause(List(q.neg)), Clause(List(q.neg, q))))
    print("\tSolving UNSAT formula ")
    println(unsat1)
    // DPLL.solve(unsat1)

    val unsat2: Formula = Formula(List(
      Clause(List(t.neg, s, q.neg)),
      Clause(List(r.neg, s.neg, p.neg)),
      Clause(List(s.neg, r.neg, q.neg)),
      Clause(List(r.neg, u, p.neg)),
      Clause(List(t.neg, p, r)),
      Clause(List(q.neg, t, r)),
      Clause(List(t.neg, r.neg, s)),
      Clause(List(p.neg, r.neg, s.neg)),
      Clause(List(p, r.neg, t)),
      Clause(List(t.neg, r, q)),
      Clause(List(s, p.neg, q)),
      Clause(List(s.neg, t.neg, q.neg)),
      // the following 2 clauses makes the formula unsat
      Clause(List(s.neg, v, q)),
      Clause(List(s.neg, v.neg, q))
    ))
    print("\tSolving UNSAT formula ")
    println(unsat2)
    // DPLL.solve(unsat2)
    true
  }
}
