needsPackage "NumericalSchubertCalculus"
setRandomSeed 2

--Problem W11^3W21^2 in G(3,7)
 --a problem with 6 solutions

print("Solving problem W11^3W21^2 in G(3,7)");

SchPblm = randomSchubertProblemInstance(
  {{2, 1},{2, 1},{1, 1},{1, 1},{1, 1}},3,7);
time S = solveSchubertProblem(SchPblm, 3,7);
assert all(S,s->checkIncidenceSolution(s, SchPblm))

 end
 ------

restart
 load"NumericalSchubertCalculus/EXA/ProblemsG37/W11^3W21^2-G37.m2"

