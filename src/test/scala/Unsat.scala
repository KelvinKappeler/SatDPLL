import stainless.collection.{List => List}
import stainless.collection.Nil

/**
  * Testing class for unsatisfiabile formulas with DPLL
  */
class Unsat extends munit.FunSuite {
  
  val a = Atom("A")
  val b = Atom("B")

  test("Not pure formula") {
    assert(!DPLL.solve(Formula(List(Clause(List(a)), Clause(List(a.neg))))).isDefined)
  }

  test("Formula 1") {
    assert(!DPLL.solve(Formula(List(Clause(List(a)), Clause(List(a.neg, b)), Clause(List(b.neg))))).isDefined)
  }

}
