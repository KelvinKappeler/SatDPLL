import stainless.collection.{List => List}
import stainless.io.StdOut.{println => println, print => print}
import stainless.collection.ListOps.FlattenableListOps
import stainless.lang.Option as Option
import stainless.lang.Some as Some
import stainless.lang.None as None
import stainless.lang.{Map => Map}
import stainless.lang.Map.ToMapOps
import stainless.annotation.*

object Test {

  implicit val state: stainless.io.State = stainless.io.newState
  
  private val q = Lit("Q")
  private val p = Lit("P")
  private val r = Lit("R")
  private val s = Lit("S")
  private val t = Lit("T")
  private val u = Lit("U")
  private val v = Lit("V")

  @ignore def testAll(): Unit = {
    testSat()
    testUnsat()
  }

  @ignore def testSat(): Unit = {
    println("Testing satisfying assignments:")

    println("=====================")
    val sat0: Formula = Formula(List(Clause(List(Lit("A")))))
    println("Solving SAT formula " + sat0.toString)
    DPLL.solve(sat0)

    println("=====================")

    val sat1: Formula = Formula(List(Clause(List(q)), Clause(List(r, r.neg, p.neg))))
    println("Solving SAT formula " + sat1.toString)
    DPLL.solve(sat1)

    println("=====================")

    val sat2: Formula = Formula(List(Clause(List(q)), Clause(List(q.neg, p.neg))))
    println("Solving SAT formula " + sat2.toString)
    println(sat2)
    DPLL.solve(sat2)

    println("=====================")

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

    println("Solving SAT formula " + sat3.toString)
    DPLL.solve(sat3)
  }

  @ignore def testUnsat(): Unit = {
    println("Testing unsatisfying assignments")

    val unsat1: Formula = Formula(List(Clause(List(q)), Clause(List(q.neg)), Clause(List(q.neg, q))))
    print("\tSolving UNSAT formula ")
    println(unsat1)
    DPLL.solve(unsat1)

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
      Clause(List(s, t, q)),
      Clause(List(u.neg)),
    ))
    print("\tSolving UNSAT formula ")
    println(unsat2)
    DPLL.solve(unsat2)
  }
}
