import stainless.collection.{List => List}
import stainless.collection.Nil

/**
  * Testing class for satisfiabile formulas with DPLL
  */
class Sat extends munit.FunSuite {
  
  val a = Atom("A")
  val b = Atom("B")
  val c = Atom("C")
  val d = Atom("D")
  val e = Atom("E")

  test("Empty clauses") {
    val emptyFormula: Formula = Formula(Nil())
    assert(DPLL.solve(emptyFormula))
  }

  test("Formula 1") {
    val sat1: Formula = Formula(List(Clause(List(a))))
    assert(DPLL.solve(sat1))
  }

  test("Formula 2") {
    val sat2: Formula = Formula(List(Clause(List(a)), Clause(List(b, b.neg, c.neg))))
    assert(DPLL.solve(sat2))
  }

  test("Formula 3") {
    val sat3: Formula = Formula(List(Clause(List(a)), Clause(List(a.neg, b.neg))))
    assert(DPLL.solve(sat3))
  }

  test("Formula 4") {
    val sat4: Formula = Formula(List(
      Clause(List(a.neg, b, c.neg)),
      Clause(List(d.neg, b.neg, e.neg)),
      Clause(List(b.neg, d.neg, c.neg)),
      Clause(List(d.neg, e.neg)),
      Clause(List(a.neg, e, d)),
      Clause(List(c.neg, a, d)),
      Clause(List(a.neg, d.neg, b)),
      Clause(List(e.neg, d.neg, b.neg)),
      Clause(List(e, d.neg, a)),
      Clause(List(a.neg, d, c)),
      Clause(List(b, e.neg, c)),
      Clause(List(b.neg, a.neg, c.neg))
    ))
    assert(DPLL.solve(sat4))
  }

}
