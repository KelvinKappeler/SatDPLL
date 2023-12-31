import stainless.io.StdOut.{println => println, print => print}

implicit val state: stainless.io.State = stainless.io.newState

object Main {
  def main(args: Array[String]): Unit = {
    val term: Term = Iff(Var("A"), Implies(Not(Or(Var("B"), Var("C"))), Var("A")))
    print(DPLL.answer(DPLL.solve(term.toFormula)))
  }
}
