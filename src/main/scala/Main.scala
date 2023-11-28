@main def main: Unit =
  val testTerm = Implies(Var("Q"), And(Var("P"), Var("R")))
  println(Transformation.toCNF(testTerm))
  println(Transformation.toClauses(testTerm))
  //DPLL.dpll(Transformation.toClauses(testTerm))
