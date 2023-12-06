import stainless.collection.List

@main def main: Unit =
  val testTerm = Implies(Var("Q"), And(Not(Var("P")), Var("R")))
  val testBasicTerm = And(Var("Q"), And(Not(Var("P")), Var("R")))
  
  val testClause1 = 
    List(
      List(VarLiteral("Q")),
      List(VarLiteral("P"), VarLiteral("R")),
      List(NotLiteral(VarLiteral("R")), NotLiteral(VarLiteral("P")))
    )
  val testClause2 =
    List(
      List(VarLiteral("Q")),
      List(NotLiteral(VarLiteral("Q")))
    )
  val testClause3 = 
  List(
    List(VarLiteral("A"), NotLiteral(VarLiteral("B")), VarLiteral("C")),
    List(NotLiteral(VarLiteral("A")), VarLiteral("D")),
    List(VarLiteral("B"), NotLiteral(VarLiteral("C")), VarLiteral("E")),
    List(NotLiteral(VarLiteral("D")), NotLiteral(VarLiteral("E"))),
    List(VarLiteral("F"), VarLiteral("G")),
    List(NotLiteral(VarLiteral("F")), NotLiteral(VarLiteral("G")), VarLiteral("H")),
    List(VarLiteral("H"), NotLiteral(VarLiteral("A")))
  )
  val unsatExample =
    List(
      List(VarLiteral("X"), VarLiteral("Y")),
      List(NotLiteral(VarLiteral("X")), NotLiteral(VarLiteral("Z"))),
      List(NotLiteral(VarLiteral("Y")), VarLiteral("W")),
      List(VarLiteral("Z"), NotLiteral(VarLiteral("W"))),
      List(VarLiteral("X"), NotLiteral(VarLiteral("Z"))),
      List(VarLiteral("X")),
      List(VarLiteral("Y"), NotLiteral(VarLiteral("W"))),
      List(VarLiteral("W"))
    )

  
  print(DPLL.solve(unsatExample))
  print("\n")
