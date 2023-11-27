@main def main: Unit =
  val testTerm = And(Or(Var("a"), Var("b")), Not(Var("c")))
  println(testTerm)
