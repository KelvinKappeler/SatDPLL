@main def main: Unit =
  val testTerm = Implies(Var("Q"), And(Not(Var("P")), Var("R")))
  val testBasicTerm = And(Var("Q"), And(Not(Var("P")), Var("R")))
  val clause = List(List(VarLiteral("Q")), List(VarLiteral("P"), VarLiteral("R")), List(NotLiteral(VarLiteral("R")), VarLiteral("P")))
  print(DPLL.solve(clause))
