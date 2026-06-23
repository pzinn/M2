-- the question is whether 'trim' should do something for inhomogeneous modules.
-- if it doesn't do enough, this test will break
gbTrace = 3
R = ZZ/7[y, x, MonomialOrder=>Lex];
I = ideal (y^3*x^2 + 2*y^2*x + 3*x*y,  3*y^2 + x*y - 3*y);
J' = saturate(I, ideal(y), MinimalGenerators => false)	    -- tell saturate not to trim
J = saturate(I, ideal(y), MinimalGenerators => true)	    -- tell saturate to trim
assert ( set flatten entries gens J === set {y-2*x-1, x^4+x^3+3*x^2+3*x} )

--

R = ZZ/32003[a..j]
I = ideal random(R^1, R^{-2,-2,-2,-2,-2,-2,-2});
trim I  -- fixed: this fails because it takes a long time...  It should stop after mingens are known to be computed:

-- see https://github.com/Macaulay2/M2/issues/450
R = QQ[]
M = subquotient ( map(R^1,R^1,1), map(R^1,R^1,1) )
N = subquotient (               , map(R^1,R^1,1) )
P = subquotient (               , map(R^1,R^1,0) )
assert ( M == 0 )
assert ( N == 0 )
assert (trim M === trim N)
assert (trim(M,Strategy=>Complement) === trim(N,Strategy=>Complement))
assert (trim P === R^1)
assert (trim(P,Strategy=>Complement) === R^1)

-- trim PID
R=QQ[x]
I = ideal(3*x^4-x^3+x^2-x-2,3*x^4+2*x^3+6*x+4)
J = trim I
assert(I == J)
assert(numgens J == 1)
I' = ideal(x^3-1, x^2+1)
J' = trim I'
assert(J'_* == {1})

-- inhomogeneous trim over ZZ[x] should not stop at a premature
-- minimal generator before detecting a unit in the ideal
R = ZZ[x]
I = ideal(x^2+x+1, x^4+x^3+x^2+x+1)
J = trim I
K = trim(I, Strategy=>Complement)
G = mingens I
H = mingens(I, Strategy=>Complement)
assert(I == ideal 1_R)
assert(J == I)
assert(K == I)
assert(ideal G == I)
assert(ideal H == I)

-- Local Variables:
-- compile-command: "make -C $M2BUILDDIR/Macaulay2/packages/Macaulay2Doc/test trim.out"
-- End:
