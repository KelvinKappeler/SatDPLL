import stainless.collection.{List => List}
import stainless.collection.Nil

/**
  * Testing class for unsatisfiabile formulas with DPLL
  */
class Unsat extends munit.FunSuite {
  
  val a = Atom("A")

  test("Formula 1") {
    val unsat1: Formula = Formula(List(Clause(List(a)), Clause(List(a.neg))))
    assert(!DPLL.solve(unsat1))
  }

}
