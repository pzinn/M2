needsPackage "NumericalSchubertCalculus"
setRandomSeed 2

--Problem W^3W2^2W11W21 in G(3,7)
 --a problem with 22 solutions

print("Solving problem W^3W2^2W11W21 in G(3,7)");

SchPblm = randomSchubertProblemInstance(
  {{2, 1},{1, 1},{2},{2},{1},{1},{1}},3,7);
time S = solveSchubertProblem(SchPblm, 3,7);
assert all(S,s->checkIncidenceSolution(s, SchPblm))

 end
 ------

restart
 load"NumericalSchubertCalculus/EXA/ProblemsG37/W^3W2^2W11W21-G37.m2"

