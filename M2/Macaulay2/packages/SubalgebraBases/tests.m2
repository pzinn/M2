-----------------------------------
-- Tests from the previous version:
-----------------------------------
-- 0) Subring tests
-- 1) Infinite generators
-- 2) simple inhomog example
-- 3) invariants of S3
-- 4) invariants of S4
-- 5) generic minors(2,2,10)
-- 6) generic minors(2,3,3)
-- 7) generic minors(2,3,4)
-- 8) example with both finite and infinite sagbi bases (infinite one)
-- 9) example with both finite and infinite sagbi bases (finite one)
-- 10) same example, with generic change of coordinates
-- 11) invariants of A3 to degree 15
-- 12) Invariants of A^1, with a nilpotent action on A^5
-- 13) Invariants of A^1, with a nilpotent action on A^3
-- 14) invariants of SL_2 on V + V + Sym^2(V)
-- 15) invariants of A^1, with a nilpotent action of A^4
-- 16) elimination order on ambient ring
-- 17) 'symmetric' quadratic artin ideal in 2x3 variables
-- * 18) toricSyz, Sturmfels example 11.19 -- Removed due to toricSyz removal
-- 19) Lex term order, simple example (verifying sagbi basis)
-- 20) Lex term order, harder example (requires new generators)
-- 21) GRevLex version of 19 (one more generator than 19)
-- 22) GRevLex version of 20 (one more generator than 20)
-- 23) Test for %
-- 24) Test for groebnerMembershipTest
-- 25) subductionQuotient
-- 26) subring intersection (infinite sagbi basis)
-- 27) subring intersection in a quotient ring (finite sagbi basis)
-- 28) Bruns/Conca example 1
-- 29) Bruns/Conca example 1

------------------------------
-- Old tests that are removed:
------------------------------

---------------------------------------------
-- Previously located between tests 7 and 8
---------------------------------------------
-- Commented out: takes too long right now --
-- with Autosubduce ~30 seconds
-- without Autosubduce ~25 seconds
---------------------------------------------
///
--generic minors(2,3,5)
genericminors = (minorsize,rowsize,colsize) -> (
    -- k by k minors of a generic m by n matrix
    matdim := rowsize * colsize - 1;
    R = ZZ/101[x_0 .. x_matdim];
    gens minors(minorsize,genericMatrix(R,x_0,rowsize,colsize)))

F = genericminors(2,3,5)
ans = matrix {{x_11*x_13-x_10*x_14, x_8*x_13-x_7*x_14, x_5*x_13-x_4*x_14, x_2*x_13-x_1*x_14,
    x_11*x_12-x_9*x_14, x_10*x_12-x_9*x_13, x_8*x_12-x_6*x_14, x_7*x_12-x_6*x_13,
    x_5*x_12-x_3*x_14, x_4*x_12-x_3*x_13, x_2*x_12-x_0*x_14, x_1*x_12-x_0*x_13,
    x_8*x_10-x_7*x_11, x_5*x_10-x_4*x_11, x_2*x_10-x_1*x_11, x_8*x_9-x_6*x_11,
    x_7*x_9-x_6*x_10, x_5*x_9-x_3*x_11, x_4*x_9-x_3*x_10, x_2*x_9-x_0*x_11, x_1*x_9-x_0*x_10,
    x_5*x_7-x_4*x_8, x_2*x_7-x_1*x_8, x_5*x_6-x_3*x_8, x_4*x_6-x_3*x_7, x_2*x_6-x_0*x_8,
    x_1*x_6-x_0*x_7, x_2*x_4-x_1*x_5, x_2*x_3-x_0*x_5, x_1*x_3-x_0*x_4,
    x_8*x_10*x_12*x_14-x_7*x_11*x_12*x_14-x_8*x_9*x_13*x_14+x_6*x_11*x_13*x_14+x_7*x_9*x_14^2-x_6*x_10*x_14^2,
    x_5*x_10*x_12*x_14-x_4*x_11*x_12*x_14-x_5*x_9*x_13*x_14+x_3*x_11*x_13*x_14+x_4*x_9*x_14^2-x_3*x_10*x_14^2,
    x_2*x_10*x_12*x_14-x_1*x_11*x_12*x_14-x_2*x_9*x_13*x_14+x_0*x_11*x_13*x_14+x_1*x_9*x_14^2-x_0*x_10*x_14^2,
    x_5*x_7*x_12*x_14-x_4*x_8*x_12*x_14-x_5*x_6*x_13*x_14+x_3*x_8*x_13*x_14+x_4*x_6*x_14^2-x_3*x_7*x_14^2,
    x_2*x_7*x_12*x_14-x_1*x_8*x_12*x_14-x_2*x_6*x_13*x_14+x_0*x_8*x_13*x_14+x_1*x_6*x_14^2-x_0*x_7*x_14^2,
    x_2*x_4*x_12*x_14-x_1*x_5*x_12*x_14-x_2*x_3*x_13*x_14+x_0*x_5*x_13*x_14+x_1*x_3*x_14^2-x_0*x_4*x_14^2,
    x_5*x_7*x_9*x_14-x_4*x_8*x_9*x_14-x_5*x_6*x_10*x_14+x_3*x_8*x_10*x_14+x_4*x_6*x_11*x_14-x_3*x_7*x_11*x_14,
    x_2*x_7*x_9*x_14-x_1*x_8*x_9*x_14-x_2*x_6*x_10*x_14+x_0*x_8*x_10*x_14+x_1*x_6*x_11*x_14-x_0*x_7*x_11*x_14,
    x_2*x_4*x_9*x_14-x_1*x_5*x_9*x_14-x_2*x_3*x_10*x_14+x_0*x_5*x_10*x_14+x_1*x_3*x_11*x_14-x_0*x_4*x_11*x_14,
    x_2*x_4*x_6*x_14-x_1*x_5*x_6*x_14-x_2*x_3*x_7*x_14+x_0*x_5*x_7*x_14+x_1*x_3*x_8*x_14-x_0*x_4*x_8*x_14,
    x_6*x_8*x_10*x_12-x_6*x_7*x_11*x_12-x_6*x_8*x_9*x_13+x_6^2*x_11*x_13+x_6*x_7*x_9*x_14-x_6^2*x_10*x_14,
    x_3*x_8*x_10*x_12-x_3*x_7*x_11*x_12-x_3*x_8*x_9*x_13+x_3*x_6*x_11*x_13+x_3*x_7*x_9*x_14-x_3*x_6*x_10*x_14,
    x_0*x_8*x_10*x_12-x_0*x_7*x_11*x_12-x_0*x_8*x_9*x_13+x_0*x_6*x_11*x_13+x_0*x_7*x_9*x_14-x_0*x_6*x_10*x_14,
    x_3*x_5*x_10*x_12-x_3*x_4*x_11*x_12-x_3*x_5*x_9*x_13+x_3^2*x_11*x_13+x_3*x_4*x_9*x_14-x_3^2*x_10*x_14,
    x_0*x_5*x_10*x_12-x_0*x_4*x_11*x_12-x_0*x_5*x_9*x_13+x_0*x_3*x_11*x_13+x_0*x_4*x_9*x_14-x_0*x_3*x_10*x_14,
    x_0*x_2*x_10*x_12-x_0*x_1*x_11*x_12-x_0*x_2*x_9*x_13+x_0^2*x_11*x_13+x_0*x_1*x_9*x_14-x_0^2*x_10*x_14,
    x_3*x_5*x_7*x_12-x_3*x_4*x_8*x_12-x_3*x_5*x_6*x_13+x_3^2*x_8*x_13+x_3*x_4*x_6*x_14-x_3^2*x_7*x_14,
    x_0*x_5*x_7*x_12-x_0*x_4*x_8*x_12-x_0*x_5*x_6*x_13+x_0*x_3*x_8*x_13+x_0*x_4*x_6*x_14-x_0*x_3*x_7*x_14,
    x_0*x_2*x_7*x_12-x_0*x_1*x_8*x_12-x_0*x_2*x_6*x_13+x_0^2*x_8*x_13+x_0*x_1*x_6*x_14-x_0^2*x_7*x_14,
    x_0*x_2*x_4*x_12-x_0*x_1*x_5*x_12-x_0*x_2*x_3*x_13+x_0^2*x_5*x_13+x_0*x_1*x_3*x_14-x_0^2*x_4*x_14,
    x_5*x_7*x_9*x_11-x_4*x_8*x_9*x_11-x_5*x_6*x_10*x_11+x_3*x_8*x_10*x_11+x_4*x_6*x_11^2-x_3*x_7*x_11^2,
    x_2*x_7*x_9*x_11-x_1*x_8*x_9*x_11-x_2*x_6*x_10*x_11+x_0*x_8*x_10*x_11+x_1*x_6*x_11^2-x_0*x_7*x_11^2,
    x_2*x_4*x_9*x_11-x_1*x_5*x_9*x_11-x_2*x_3*x_10*x_11+x_0*x_5*x_10*x_11+x_1*x_3*x_11^2-x_0*x_4*x_11^2,
    x_2*x_4*x_6*x_11-x_1*x_5*x_6*x_11-x_2*x_3*x_7*x_11+x_0*x_5*x_7*x_11+x_1*x_3*x_8*x_11-x_0*x_4*x_8*x_11,
    x_3*x_5*x_7*x_9-x_3*x_4*x_8*x_9-x_3*x_5*x_6*x_10+x_3^2*x_8*x_10+x_3*x_4*x_6*x_11-x_3^2*x_7*x_11,
    x_0*x_5*x_7*x_9-x_0*x_4*x_8*x_9-x_0*x_5*x_6*x_10+x_0*x_3*x_8*x_10+x_0*x_4*x_6*x_11-x_0*x_3*x_7*x_11,
    x_0*x_2*x_7*x_9-x_0*x_1*x_8*x_9-x_0*x_2*x_6*x_10+x_0^2*x_8*x_10+x_0*x_1*x_6*x_11-x_0^2*x_7*x_11,
    x_0*x_2*x_4*x_9-x_0*x_1*x_5*x_9-x_0*x_2*x_3*x_10+x_0^2*x_5*x_10+x_0*x_1*x_3*x_11-x_0^2*x_4*x_11,
    x_2*x_4*x_6*x_8-x_1*x_5*x_6*x_8-x_2*x_3*x_7*x_8+x_0*x_5*x_7*x_8+x_1*x_3*x_8^2-x_0*x_4*x_8^2,
    x_0*x_2*x_4*x_6-x_0*x_1*x_5*x_6-x_0*x_2*x_3*x_7+x_0^2*x_5*x_7+x_0*x_1*x_3*x_8-x_0^2*x_4*x_8}};
assert(
    time subalgebraBasis(F,Limit=>100)
    ==
    ans)
assert(
    time subalgebraBasis(F,Limit=>100,AutoSubduce=>false)
    ==
    ans)
///
---------------------------------------------

-------------------------------------------------------
-- Previously located between tests 14 and 15
-------------------------------------------------------
-- Commented out: takes too long right now ~12 seconds
-------------------------------------------------------

-- invariants of A3 to degree 30
///
kk = ZZ/101
R = kk[a,b,c]
ans = matrix {{a+b+c, a*b+a*c+b*c, a*b*c, a*b^2+a^2*c+b*c^2, a*b^3+a^3*c+b*c^3, a*b^4+a^4*c+b*c^4,
    a*b^5+a^5*c+b*c^5, a*b^6+a^6*c+b*c^6, a*b^7+a^7*c+b*c^7, a*b^8+a^8*c+b*c^8, a*b^9+a^9*c+b*c^9,
    a*b^10+a^10*c+b*c^10, a*b^11+a^11*c+b*c^11, a*b^12+a^12*c+b*c^12, a*b^13+a^13*c+b*c^13,
    a*b^14+a^14*c+b*c^14, a*b^15+a^15*c+b*c^15, a*b^16+a^16*c+b*c^16, a*b^17+a^17*c+b*c^17,
    a*b^18+a^18*c+b*c^18, a*b^19+a^19*c+b*c^19, a*b^20+a^20*c+b*c^20, a*b^21+a^21*c+b*c^21,
    a*b^22+a^22*c+b*c^22, a*b^23+a^23*c+b*c^23, a*b^24+a^24*c+b*c^24, a*b^25+a^25*c+b*c^25,
    a*b^26+a^26*c+b*c^26, a*b^27+a^27*c+b*c^27, a*b^28+a^28*c+b*c^28, a*b^29+a^29*c+b*c^29}}
F = matrix{{a+b+c, a*b+b*c+c*a, a*b*c, a^2*b+b^2*c+c^2*a}}
assert(
    time subalgebraBasis(F,Limit=>30)
    ==
    ans)
///
--------------------------------------------------

----------------------------------------
-- toricSyz has been removed
----------------------------------------
-- 18) toricSyz, Sturmfels example 11.19
-- TEST ///
-- R = QQ[t_1,t_2];
-- A = subring sagbi{t_1^2,t_1*t_2,t_2^2};
-- M = matrix{{t_1^2, t_1*t_2}};
-- assert(toricSyz(A, M)==matrix {{-t_2^2, t_1*t_2}, {-t_1*t_2, t_1^2}});
-- ///
----------------------------------------------------

-- 0) Subring tests
TEST ///
R = QQ[x1, x2, x3];
S = QQ[e1, e2, e3, y];
f = map(R, S, {x1 + x2 + x3, x1*x2 + x1*x3 + x2*x3, x1*x2*x3,
(x1 - x2)*(x1 - x3)*(x2 - x3)});
A = subring matrix f;
assert(not isSAGBI A)
///
---------------------


invariantsSn = (n) -> (
    -- ring of invariants of S_n
    x := getSymbol "x";
    R := ZZ/101[x_0 .. x_(n-1)];
    map(R^1, n, (j,i) -> sum apply(toList(x_0 .. x_(n-1)), x->x^(i+1))))

genericminors = (minorsize,rowsize,colsize) -> (
    -- k by k minors of a generic m by n matrix
    matdim := rowsize * colsize - 1;
    x := getSymbol "x";
    R := ZZ/101[x_0 .. x_matdim];
    gens minors(minorsize,genericMatrix(R,x_0,rowsize,colsize)))

-- 1) Infinite generators
TEST ///
R=QQ[x,y,MonomialOrder=>Lex]
M=matrix{{x+y,x*y,x*y^2}}
assert(subalgebraBasis(M,Limit=>3)==M)
///
--------------------------------------------

-- 2) simple inhomog example
TEST ///
kk = ZZ/101
R = kk[a,b,c]
F = matrix{{a+b+c-1, a^2+b^2+c^2-a, a^3+b^3+c^3-b}}
ans = matrix {{a+b+c-1, a*b+a*c+b*c+50*b+50*c, a*b*c+50*b^2+50*b*c+50*c^2-9*b+25*c}}
assert(
    time subalgebraBasis(F,Limit=>3,AutoSubduce=>false)
    ==
    ans)
///
--------------------------------------------

-- 3) invariants of S3
TEST ///
invariantsSn = (n) -> (
    -- ring of invariants of S_n
    R = ZZ/101[x_0 .. x_(n-1)];
    map(R^1, n, (j,i) -> sum apply(toList(x_0 .. x_(n-1)), x->x^(i+1))))

F = invariantsSn 3
ans = matrix {{x_0+x_1+x_2, x_0*x_1+x_0*x_2+x_1*x_2, x_0*x_1*x_2}}
assert(
    time subalgebraBasis(F,Limit=>10)
    ==
    ans)
///
--------------------------------------------


-- 4) invariants of S4
TEST ///
invariantsSn = (n) -> (
    -- ring of invariants of S_n
    R = ZZ/101[x_0 .. x_(n-1)];
    map(R^1, n, (j,i) -> sum apply(toList(x_0 .. x_(n-1)), x->x^(i+1))))

F = invariantsSn 4
ans = matrix {{x_0+x_1+x_2+x_3,
    x_0*x_1+x_0*x_2+x_1*x_2+x_0*x_3+x_1*x_3+x_2*x_3,
    x_0*x_1*x_2+x_0*x_1*x_3+x_0*x_2*x_3+x_1*x_2*x_3,
    x_0*x_1*x_2*x_3}}
assert(
    time subalgebraBasis(F,Limit=>10)
    ==
    ans)
///
--------------------------------------------

-- 5) generic minors(2,2,10)
TEST ///
genericminors = (minorsize,rowsize,colsize) -> (
    -- k by k minors of a generic m by n matrix
    matdim := rowsize * colsize - 1;
    R = ZZ/101[x_0 .. x_matdim];
    gens minors(minorsize,genericMatrix(R,x_0,rowsize,colsize)))

F = genericminors(2,2,10)
ans = matrix {{x_17*x_18-x_16*x_19, x_15*x_18-x_14*x_19, x_13*x_18-x_12*x_19,
    x_11*x_18-x_10*x_19, x_9*x_18-x_8*x_19, x_7*x_18-x_6*x_19, x_5*x_18-x_4*x_19,
    x_3*x_18-x_2*x_19, x_1*x_18-x_0*x_19, x_15*x_16-x_14*x_17, x_13*x_16-x_12*x_17,
    x_11*x_16-x_10*x_17, x_9*x_16-x_8*x_17, x_7*x_16-x_6*x_17, x_5*x_16-x_4*x_17,
    x_3*x_16-x_2*x_17, x_1*x_16-x_0*x_17, x_13*x_14-x_12*x_15, x_11*x_14-x_10*x_15,
    x_9*x_14-x_8*x_15, x_7*x_14-x_6*x_15, x_5*x_14-x_4*x_15, x_3*x_14-x_2*x_15,
    x_1*x_14-x_0*x_15, x_11*x_12-x_10*x_13, x_9*x_12-x_8*x_13, x_7*x_12-x_6*x_13,
    x_5*x_12-x_4*x_13, x_3*x_12-x_2*x_13, x_1*x_12-x_0*x_13, x_9*x_10-x_8*x_11,
    x_7*x_10-x_6*x_11, x_5*x_10-x_4*x_11, x_3*x_10-x_2*x_11, x_1*x_10-x_0*x_11,
    x_7*x_8-x_6*x_9, x_5*x_8-x_4*x_9, x_3*x_8-x_2*x_9, x_1*x_8-x_0*x_9, x_5*x_6-x_4*x_7,
    x_3*x_6-x_2*x_7, x_1*x_6-x_0*x_7, x_3*x_4-x_2*x_5, x_1*x_4-x_0*x_5, x_1*x_2-x_0*x_3}}
assert(
    time subalgebraBasis(F,Limit=>100, AutoSubduce=>false)
    ==
    ans)
///
--------------------------------------------

-- 6) generic minors(2,3,3)
TEST ///
genericminors = (minorsize,rowsize,colsize) -> (
    -- k by k minors of a generic m by n matrix
    matdim := rowsize * colsize - 1;
    R = ZZ/101[x_0 .. x_matdim];
    gens minors(minorsize,genericMatrix(R,x_0,rowsize,colsize)))

F = genericminors(2,3,3)
ans = matrix {{x_5*x_7-x_4*x_8, x_2*x_7-x_1*x_8, x_5*x_6-x_3*x_8, x_4*x_6-x_3*x_7,
    x_2*x_6-x_0*x_8, x_1*x_6-x_0*x_7, x_2*x_4-x_1*x_5, x_2*x_3-x_0*x_5, x_1*x_3-x_0*x_4,
    x_2*x_4*x_6*x_8-x_1*x_5*x_6*x_8-x_2*x_3*x_7*x_8+x_0*x_5*x_7*x_8+x_1*x_3*x_8^2-x_0*x_4*x_8^2,
    x_0*x_2*x_4*x_6-x_0*x_1*x_5*x_6-x_0*x_2*x_3*x_7+x_0^2*x_5*x_7+x_0*x_1*x_3*x_8-x_0^2*x_4*x_8}}
assert(
    time subalgebraBasis(F,Limit=>100)
    ==
    ans)
///
--------------------------------------------

-- 7) generic minors(2,3,4)
TEST ///
genericminors = (minorsize,rowsize,colsize) -> (
    -- k by k minors of a generic m by n matrix
    matdim := rowsize * colsize - 1;
    R = ZZ/101[x_0 .. x_matdim];
    gens minors(minorsize,genericMatrix(R,x_0,rowsize,colsize)))

F = genericminors(2,3,4)
ans = matrix {{x_8*x_10-x_7*x_11, x_5*x_10-x_4*x_11, x_2*x_10-x_1*x_11, x_8*x_9-x_6*x_11,
    x_7*x_9-x_6*x_10, x_5*x_9-x_3*x_11, x_4*x_9-x_3*x_10, x_2*x_9-x_0*x_11, x_1*x_9-x_0*x_10,
    x_5*x_7-x_4*x_8, x_2*x_7-x_1*x_8, x_5*x_6-x_3*x_8, x_4*x_6-x_3*x_7, x_2*x_6-x_0*x_8,
    x_1*x_6-x_0*x_7, x_2*x_4-x_1*x_5, x_2*x_3-x_0*x_5, x_1*x_3-x_0*x_4,
    x_5*x_7*x_9*x_11-x_4*x_8*x_9*x_11-x_5*x_6*x_10*x_11+x_3*x_8*x_10*x_11+x_4*x_6*x_11^2-x_3*x_7*x_11^2,
    x_2*x_7*x_9*x_11-x_1*x_8*x_9*x_11-x_2*x_6*x_10*x_11+x_0*x_8*x_10*x_11+x_1*x_6*x_11^2-x_0*x_7*x_11^2,
    x_2*x_4*x_9*x_11-x_1*x_5*x_9*x_11-x_2*x_3*x_10*x_11+x_0*x_5*x_10*x_11+x_1*x_3*x_11^2-x_0*x_4*x_11^2,
    x_2*x_4*x_6*x_11-x_1*x_5*x_6*x_11-x_2*x_3*x_7*x_11+x_0*x_5*x_7*x_11+x_1*x_3*x_8*x_11-x_0*x_4*x_8*x_11,
    x_3*x_5*x_7*x_9-x_3*x_4*x_8*x_9-x_3*x_5*x_6*x_10+x_3^2*x_8*x_10+x_3*x_4*x_6*x_11-x_3^2*x_7*x_11,
    x_0*x_5*x_7*x_9-x_0*x_4*x_8*x_9-x_0*x_5*x_6*x_10+x_0*x_3*x_8*x_10+x_0*x_4*x_6*x_11-x_0*x_3*x_7*x_11,
    x_0*x_2*x_7*x_9-x_0*x_1*x_8*x_9-x_0*x_2*x_6*x_10+x_0^2*x_8*x_10+x_0*x_1*x_6*x_11-x_0^2*x_7*x_11,
    x_0*x_2*x_4*x_9-x_0*x_1*x_5*x_9-x_0*x_2*x_3*x_10+x_0^2*x_5*x_10+x_0*x_1*x_3*x_11-x_0^2*x_4*x_11,
    x_2*x_4*x_6*x_8-x_1*x_5*x_6*x_8-x_2*x_3*x_7*x_8+x_0*x_5*x_7*x_8+x_1*x_3*x_8^2-x_0*x_4*x_8^2,
    x_0*x_2*x_4*x_6-x_0*x_1*x_5*x_6-x_0*x_2*x_3*x_7+x_0^2*x_5*x_7+x_0*x_1*x_3*x_8-x_0^2*x_4*x_8}}
assert(
    time subalgebraBasis(F,Limit=>100)
    ==
    ans)
///

--------------------------------------------
-- 8) example with both finite and infinite sagbi bases (infinite one)
TEST ///
kk = ZZ/101
R = kk[symbol x,symbol y]   -- x>y gives infinite, y>x gives finite
F = matrix{{x, x*y-y^2, x*y^2}}
ans = matrix {{x, x*y-y^2, x*y^2, x*y^3+50*y^4, x*y^4, x*y^5-34*y^6, x*y^6, x*y^7+25*y^8, x*y^8,
    x*y^9+20*y^10, x*y^10, x*y^11-17*y^12, x*y^12, x*y^13-29*y^14, x*y^14, x*y^15-38*y^16, x*y^16,
    x*y^17-45*y^18, x*y^18, x*y^19+10*y^20, x*y^20, x*y^21-46*y^22, x*y^22, x*y^23+42*y^24, x*y^24,
    x*y^25+31*y^26, x*y^26, x*y^27+36*y^28, x*y^28, x*y^29-27*y^30}}
assert(
    time subalgebraBasis(F,Limit=>30)
    ==
    ans)
///
--------------------------------------------

-- 9) example with both finite and infinite sagbi bases (finite one)
TEST ///
kk = ZZ/101
R = kk[symbol y,symbol x]   -- x>y gives infinite, y>x gives finite
F = matrix{{x, x*y-y^2, x*y^2}}
ans = matrix {{x, y^2-y*x, y*x^2}}
assert(
    time subalgebraBasis(F,Limit=>1000)
    ==
    ans)
///
--------------------------------------------

-- 10) same example, with generic change of coordinates
TEST ///
kk = ZZ/101
R = kk[symbol x,symbol y]   -- Change of coordinates (i.e. random term order)
F = matrix{{x, x*y-y^2, x*y^2}}
--G = random(R^1, R^(elements(2:-1)))
G = matrix {{43*x+49*y, -37*x-39*y}}
Coordchange = map(R, R, G)
ans = matrix {{x-20*y, x*y+35*y^2, x*y^2-20*y^3, x*y^3-43*y^4, x*y^4-20*y^5, x*y^5+32*y^6,
    x*y^6-20*y^7, x*y^7+19*y^8, x*y^8-20*y^9, x*y^9-9*y^10, x*y^10-20*y^11, x*y^11+6*y^12,
    x*y^12-20*y^13, x*y^13-41*y^14, x*y^14-20*y^15, x*y^15+50*y^16, x*y^16-20*y^17,
    x*y^17+31*y^18, x*y^18-20*y^19, x*y^19+36*y^20, x*y^20-20*y^21, x*y^21-15*y^22,
    x*y^22-20*y^23, x*y^23-7*y^24, x*y^24-20*y^25, x*y^25-8*y^26, x*y^26-20*y^27,
    x*y^27+20*y^28, x*y^28-20*y^29, x*y^29-50*y^30}}
F = Coordchange F
assert(
    time subalgebraBasis(F,Limit=>30)
    ==
    ans)
///
--------------------------------------------

-- 11) invariants of A3 to degree 15
TEST ///
-- invariants of A3, infinite sagbi bases, at least for lex order
-- it is infinite for all term orders.
kk = ZZ/101
R = kk[a,b,c]
ans = matrix {{a+b+c, a*b+a*c+b*c, a*b*c, a*b^2+a^2*c+b*c^2, a*b^3+a^3*c+b*c^3,
    a*b^4+a^4*c+b*c^4, a*b^5+a^5*c+b*c^5, a*b^6+a^6*c+b*c^6, a*b^7+a^7*c+b*c^7,
    a*b^8+a^8*c+b*c^8, a*b^9+a^9*c+b*c^9, a*b^10+a^10*c+b*c^10, a*b^11+a^11*c+b*c^11,
    a*b^12+a^12*c+b*c^12, a*b^13+a^13*c+b*c^13, a*b^14+a^14*c+b*c^14}}
F = matrix{{a+b+c, a*b+b*c+c*a, a*b*c, a^2*b+b^2*c+c^2*a}}
assert(
    time subalgebraBasis(F,Limit=>15)
    ==
    ans)
///
-------------------------------------------

-- 12) Invariants of A^1, with a nilpotent action on A^5
TEST ///
x = symbol x
kk = ZZ/101
R = kk[t,x_1..x_5, MonomialOrder=>Lex, Degrees=>{1,5,4,3,2,1}]
ans = matrix {{x_5, t*x_5+x_4, t^2*x_5+2*t*x_4+2*x_3, x_3*x_5+50*x_4^2,
    t^3*x_5+3*t^2*x_4+6*t*x_3+6*x_2,
    t*x_3*x_5+50*t*x_4^2-49*x_2*x_5+50*x_3*x_4, t^4*x_5+4*t^3*x_4+12*t^2*x_3+24*t*x_2+24*x_1,
    x_2*x_5^2-x_3*x_4*x_5+34*x_4^3, x_1*x_5-x_2*x_4-50*x_3^2,
    t^2*x_3*x_5+50*t^2*x_4^2+3*t*x_2*x_5-t*x_3*x_4+3*x_2*x_4-2*x_3^2,
    t*x_2*x_5^2-t*x_3*x_4*x_5+34*t*x_4^3+x_2*x_4*x_5-35*x_3^2*x_5+34*x_3*x_4^2,
    t^3*x_3*x_5+50*t^3*x_4^2-46*t^2*x_2*x_5+49*t^2*x_3*x_4+
    6*t*x_1*x_5+3*t*x_2*x_4-3*t*x_3^2+6*x_1*x_4-3*x_2*x_3,
    t^2*x_2*x_5^2-t^2*x_3*x_4*x_5+34*t^2*x_4^3+2*t*x_2*x_4*x_5+
    31*t*x_3^2*x_5-33*t*x_3*x_4^2-2*x_2*x_3*x_5+2*x_2*x_4^2+33*x_3^2*x_4,
    t^4*x_3*x_5+50*t^4*x_4^2+6*t^3*x_2*x_5-2*t^3*x_3*x_4+12*t^2*x_1*x_5+
    6*t^2*x_2*x_4-6*t^2*x_3^2+24*t*x_1*x_4-12*t*x_2*x_3+24*x_1*x_3-18*x_2^2,
    x_1*x_3*x_5+50*x_1*x_4^2-26*x_2^2*x_5-50*x_2*x_3*x_4-17*x_3^3,
    t^3*x_2*x_5^2-t^3*x_3*x_4*x_5+34*t^3*x_4^3+3*t^2*x_2*x_4*x_5-4*t^2*x_3^2*x_5+
    t^2*x_3*x_4^2-6*t*x_2*x_3*x_5+6*t*x_2*x_4^2-2*t*x_3^2*x_4-6*x_2^2*x_5+6*x_2*x_3*x_4+31*x_3^3,
    x_2^2*x_5^2-2*x_2*x_3*x_4*x_5-33*x_2*x_4^3-44*x_3^3*x_5-34*x_3^2*x_4^2,
    t^4*x_2*x_5^2-t^4*x_3*x_4*x_5+34*t^4*x_4^3+4*t^3*x_2*x_4*x_5-39*t^3*x_3^2*x_5+
    35*t^3*x_3*x_4^2-12*t^2*x_2*x_3*x_5+12*t^2*x_2*x_4^2-4*t^2*x_3^2*x_4-
    16*t*x_1*x_3*x_5+8*t*x_1*x_4^2-12*t*x_2^2*x_5+16*t*x_2*x_3*x_4-8*t*x_3^3-
    24*x_1*x_2*x_5+8*x_1*x_3*x_4+12*x_2^2*x_4-8*x_2*x_3^2,
    t*x_2^2*x_5^2-2*t*x_2*x_3*x_4*x_5-33*t*x_2*x_4^3-44*t*x_3^3*x_5-34*t*x_3^2*x_4^2+
    2*x_1*x_2*x_5^2-2*x_1*x_3*x_4*x_5-33*x_1*x_4^3-x_2^2*x_4*x_5+x_2*x_3^2*x_5-45*x_3^3*x_4,
    t^5*x_2*x_5^2-t^5*x_3*x_4*x_5+34*t^5*x_4^3+5*t^4*x_2*x_4*x_5+27*t^4*x_3^2*x_5-
    32*t^4*x_3*x_4^2-20*t^3*x_2*x_3*x_5+20*t^3*x_2*x_4^2+27*t^3*x_3^2*x_4-
    40*t^2*x_1*x_3*x_5+20*t^2*x_1*x_4^2-30*t^2*x_2^2*x_5+40*t^2*x_2*x_3*x_4-20*t^2*x_3^3-
    19*t*x_1*x_2*x_5+40*t*x_1*x_3*x_4-41*t*x_2^2*x_4-40*t*x_2*x_3^2+5*x_1^2*x_5-29*x_1*x_2*x_4-
    16*x_1*x_3^2-12*x_2^2*x_3,
    t^2*x_2^2*x_5^2-2*t^2*x_2*x_3*x_4*x_5-33*t^2*x_2*x_4^3-44*t^2*x_3^3*x_5-
    34*t^2*x_3^2*x_4^2+4*t*x_1*x_2*x_5^2-4*t*x_1*x_3*x_4*x_5+35*t*x_1*x_4^3-
    2*t*x_2^2*x_4*x_5+2*t*x_2*x_3^2*x_5+11*t*x_3^3*x_4+4*x_1*x_2*x_4*x_5-
    39*x_1*x_3^2*x_5+35*x_1*x_3*x_4^2+2*x_2^2*x_3*x_5-4*x_2^2*x_4^2+37*x_2*x_3^2*x_4+44*x_3^4,
    t^6*x_2*x_5^2-t^6*x_3*x_4*x_5+34*t^6*x_4^3+4*t^5*x_1*x_5^2+2*t^5*x_2*x_4*x_5-
    6*t^5*x_3^2*x_5+2*t^5*x_3*x_4^2+20*t^4*x_1*x_4*x_5-30*t^4*x_2*x_3*x_5+
    10*t^4*x_2*x_4^2+40*t^3*x_1*x_4^2+41*t^3*x_2^2*x_5-19*t^2*x_1*x_2*x_5+
    19*t^2*x_1*x_3*x_4+41*t^2*x_2^2*x_4+5*t*x_1^2*x_5-48*t*x_1*x_2*x_4+
    43*t*x_1*x_3^2+29*t*x_2^2*x_3+5*x_1^2*x_4+43*x_1*x_2*x_3+29*x_2^3,
    t^3*x_2^2*x_5^2-2*t^3*x_2*x_3*x_4*x_5-33*t^3*x_2*x_4^3-44*t^3*x_3^3*x_5-
    34*t^3*x_3^2*x_4^2+6*t^2*x_1*x_2*x_5^2-6*t^2*x_1*x_3*x_4*x_5+2*t^2*x_1*x_4^3-
    3*t^2*x_2^2*x_4*x_5+3*t^2*x_2*x_3^2*x_5-34*t^2*x_3^3*x_4+8*t*x_1^2*x_5^2-
    4*t*x_1*x_2*x_4*x_5-8*t*x_1*x_3^2*x_5+4*t*x_1*x_3*x_4^2+6*t*x_2^2*x_3*x_5-
    4*t*x_2^2*x_4^2+2*t*x_2*x_3^2*x_4+33*t*x_3^4+8*x_1^2*x_4*x_5-12*x_1*x_2*x_3*x_5-
    4*x_1*x_2*x_4^2+4*x_1*x_3^2*x_4+6*x_2^3*x_5+33*x_2*x_3^3,
    t^4*x_2^2*x_5^2-2*t^4*x_2*x_3*x_4*x_5-33*t^4*x_2*x_4^3-44*t^4*x_3^3*x_5-
    34*t^4*x_3^2*x_4^2+8*t^3*x_1*x_2*x_5^2-8*t^3*x_1*x_3*x_4*x_5-31*t^3*x_1*x_4^3-
    4*t^3*x_2^2*x_4*x_5+4*t^3*x_2*x_3^2*x_5+22*t^3*x_3^3*x_4+16*t^2*x_1^2*x_5^2-
    8*t^2*x_1*x_2*x_4*x_5-16*t^2*x_1*x_3^2*x_5+8*t^2*x_1*x_3*x_4^2+12*t^2*x_2^2*x_3*x_5-
    8*t^2*x_2^2*x_4^2+4*t^2*x_2*x_3^2*x_4-35*t^2*x_3^4+32*t*x_1^2*x_4*x_5-
    48*t*x_1*x_2*x_3*x_5-16*t*x_1*x_2*x_4^2+16*t*x_1*x_3^2*x_4+24*t*x_2^3*x_5+
    31*t*x_2*x_3^3+16*x_1^2*x_4^2-48*x_1*x_2*x_3*x_4-46*x_1*x_3^3+24*x_2^3*x_4-12*x_2^2*x_3^2}}
F = matrix{{x_5, t*x_5+x_4, t^2*x_5+2*t*x_4+2*x_3, t^3*x_5+3*t^2*x_4+6*t*x_3+6*x_2,
    t^4*x_5+4*t^3*x_4+12*t^2*x_3+24*t*x_2+24*x_1}}
assert(
    time subalgebraBasis(F,Limit=>30)
    ==
    ans)
///
--------------------------------------------

-- 13) Invariants of A^1, with a nilpotent action on A^3
TEST ///
x = symbol x;
t = symbol t;
kk = ZZ/101
--R = kk[t,x_1,x_2,x_3, MonomialOrder=>Lex];
R = kk[t,x_1..x_3, MonomialOrder=>Lex, Degrees=>{1,3,2,1}]
ans = matrix {{x_3, t*x_3+x_2, t^2*x_3+2*t*x_2+2*x_1, x_1*x_3+50*x_2^2}}
F = matrix{{x_3, t*x_3+x_2, t^2*x_3+2*t*x_2+2*x_1}}
assert(
    time subalgebraBasis(F,Limit=>200)
    ==
    ans)
///
--------------------------------------------

-- 14) invariants of SL_2 on V + V + Sym^2(V)
TEST ///
u = symbol u;
v = symbol v;
s = symbol s;
kk = ZZ/101
R = kk[u_1,u_2,v_1,v_2,s_0,s_1,s_2];
ans = matrix {{s_1^2-4*s_0*s_2, u_2*v_1-u_1*v_2, v_2^2*s_0-v_1*v_2*s_1+v_1^2*s_2,
    u_2*v_2*s_0+50*u_2*v_1*s_1+50*u_1*v_2*s_1+u_1*v_1*s_2, u_2^2*s_0-u_1*u_2*s_1+u_1^2*s_2}}
F = matrix{{u_2*v_1-u_1*v_2, s_1^2-4*s_0*s_2, s_0*u_2^2+s_2*u_1^2-s_1*u_1*u_2,
    s_0*v_2^2+s_2*v_1^2-s_1*v_1*v_2, 2*s_0*u_2*v_2+2*s_2*u_1*v_1-s_1*(u_2*v_1+u_1*v_2)}}
assert(
    time subalgebraBasis(F,Limit=>30)
    ==
    ans)
///
------------------------------------------------

-- 15) invariants of A^1, with a nilpotent action of A^4
TEST ///
x = symbol x
kk = ZZ/101
R = kk[t,x_1..x_4, MonomialOrder=>Lex, Degrees=>{1,4,3,2,1}]
F = matrix{{x_4, t*x_4+x_3, t^2*x_4+2*t*x_3+2*x_2, t^3*x_4+3*t^2*x_3+6*t*x_2+6*x_1}}
ans = matrix {{x_4, t*x_4+x_3, t^2*x_4+2*t*x_3+2*x_2, x_2*x_4+50*x_3^2, t^3*x_4+3*t^2*x_3+6*t*x_2+6*x_1,
    t*x_2*x_4+50*t*x_3^2-49*x_1*x_4+50*x_2*x_3, x_1*x_4^2-x_2*x_3*x_4+34*x_3^3,
    t^2*x_2*x_4+50*t^2*x_3^2+3*t*x_1*x_4-t*x_2*x_3+3*x_1*x_3-2*x_2^2,
    t*x_1*x_4^2-t*x_2*x_3*x_4+34*t*x_3^3+x_1*x_3*x_4-35*x_2^2*x_4+34*x_2*x_3^2,
    t^2*x_1*x_4^2-t^2*x_2*x_3*x_4+34*t^2*x_3^3+2*t*x_1*x_3*x_4+
    31*t*x_2^2*x_4-33*t*x_2*x_3^2-2*x_1*x_2*x_4+2*x_1*x_3^2+33*x_2^2*x_3,
    t^3*x_1*x_4^2-t^3*x_2*x_3*x_4+34*t^3*x_3^3+3*t^2*x_1*x_3*x_4-
    4*t^2*x_2^2*x_4+t^2*x_2*x_3^2-6*t*x_1*x_2*x_4+6*t*x_1*x_3^2-2*t*x_2^2*x_3-
    6*x_1^2*x_4+6*x_1*x_2*x_3+31*x_2^3,
    x_1^2*x_4^2-2*x_1*x_2*x_3*x_4-33*x_1*x_3^3-44*x_2^3*x_4-34*x_2^2*x_3^2}}
assert(
    time subalgebraBasis(F,Limit=>30)
    ==
    ans)
///
----------------------------------------------------

-- 16) elimination order on ambient ring
TEST ///
BaseRing = QQ[y, x, MonomialOrder=>{Eliminate 1, GRevLex}]
F = matrix{{x, x*y-y^2, x*y^2}}
ans = matrix {{x, y^2-y*x, y*x^2}}
assert(
    time subalgebraBasis(F,Limit=>1000)
    == ans)
///
-----------------------------------------------------

-- 17) 'symmetric' quadratic artin ideal in 2x3 variables
TEST ///
kk = ZZ/101
R = kk[symbol a..symbol f]
F = mingens ((ideal(a,b,c))^2 + (ideal(d,e,f))^2 + (ideal(a+d,b+e,c+f))^2)
ans = matrix {{f^2, e*f, d*f, c*f, e^2, d*e, c*e+b*f, b*e, d^2,
    c*d+a*f, b*d+a*e, a*d, c^2, b*c, a*c, b^2, a*b, a^2, b*f^3, a*f^3,
    a*e*f^2, a*e^2*f, b^3*f, a*b^2*f, a^2*b*f, a^3*f, a*e^3, a^3*e}}
assert(
    time subalgebraBasis(F,Limit=>100)
    ==
    ans)
///
----------------------------------------------------

-- 18) Lex term order simple
--
-- These generators already form a sagbi basis
-- The sagbi algorithm should check for S-pairs up and including degree 10
TEST ///
R = QQ[x, y, MonomialOrder => Lex]
S = subring({x*y - y, x - y^2, y^3})
ans = matrix {{x - y^2, x*y - y, y^3}}
assert(
    time subalgebraBasis(S,Limit=>30)
    ==
    ans
    )
///
----------------------------------------------------

-- 19) Lex term order harder
-- Similar generating set to TEST 20
-- Should test for S-pairs up to degree 23
TEST ///
R = QQ[x, y, MonomialOrder => Lex]
S = subring({x^2,x*y-y,2*x+y^3,y^4})
ans =  matrix {{x+(1/2)*y^3, x*y-y, y^4, x*y^3+(1/4)*y^6,
    x*y^6+(2/9)*y^9-(1/3)*y^6-(4/9)*y^3, y^6-4*y^3, y^9-8*y^3}}
assert(
    time subalgebraBasis(S,Limit=>30)
    ==
    ans
    )
///
----------------------------------------------------

-- 20) GRevLex version of 18
TEST ///
R = QQ[x, y];
S = subring({x*y-y,2*x+y^3,y^4});
ans = matrix {{x*y-y, y^3+2*x, y^4, x^4-4*x^3+6*x^2-4*x}}
assert(
    time subalgebraBasis(S,Limit=>30)
    ==
    ans
    )
///
----------------------------------------------------

-- 21) GRevLex version of 19
TEST ///
R = QQ[x, y]
S = subring({x^2,x*y-y,2*x+y^3,y^4})
ans = matrix {{x*y-y, x^2, y^3+2*x, y^4, x^4*y-y, x*y^6-4*x*y^3-12*x^3-4*x,
    x^5-2*x*y^3-4*x^3-x, x*y^3+2*x^3, x^3+x}}
assert(
    time subalgebraBasis(S,Limit=>30)
    ==
    ans
    )
///
----------------------------------------------------

-- 22) Test for %
TEST ///
R = QQ[x, y];
S = subring({x+y, x*y, x*y^2});
f1 = x+y + 2*x*y;
f2 = x+2*y;
assert(
    time (f1 % S)
    ==
    0
    )
assert(
    time (f2 % S)
    ==
    y)
///
----------------------------------------------------

-- 23) Test for groebnerMembershipTest
TEST ///
R = QQ[x, y];
S = subring({x+y, x*y, x*y^2});
f1 = x+y + 2*x*y;
f2 = x+2*y;
assert(
    time groebnerMembershipTest(f1,S)
    )
assert(
    time not groebnerMembershipTest(f2,S)
    )
///
----------------------------------------------------

-- 24) subductionQuotient
TEST ///
R = QQ[x,y];
S = subring {x+y, x*y, x*y^2};
f = x^5;
subQuot = f // S;
Q = ring subQuot;
m = map(R, Q, gens S);
ans = x^5+x^4*y+4*x^3*y^2+6*x^2*y^3+4*x*y^4+y^5;
assert(
    (m subQuot)
    ==
    ans
    )
assert(
    (m subQuot) + (f % S)
    ==
    f
    )
///
----------------------------------------------------

-- 25) subring intersection (infinite sagbi basis)
TEST ///
R = QQ[x,y];
S1 = subring {x^2, x*y};
S2 = subring {x^3, y};
S12 = intersect(S1, S2, Limit => 8, CheckFullIntersection => false);
ans = matrix {{x^3*y, x^3*y^3, x^6}}
assert(
    gens S12
    ==
    ans
    )
///
----------------------------------------------------

-- 26) subring intersection in a quotient ring (finite sagbi basis)
TEST ///
R = QQ[x,y];
I = ideal(x^3 + x*y^2 + y^3);
Q = R/I;
S1 = subring {x^2, x*y};
S2 = subring {x, y^2};
S12 = intersect(S1, S2, Limit => 20);
ans = matrix {{x^2, x^2*y^2, y^4, x*y^3, y^6, x*y^5}}
assert(
    isSAGBI S12
    )
assert(
    gens sagbi S12
    ==
    ans)
///
----------------------------------------------------

-- 27) Bruns/Conca example 1
TEST ///
R = QQ[x,y,z]
M = matrix{{x^3+y^3+z^3,   x^4+y^4+z^4,   x^5+y^5+z^5}}
partialSBSeventeen = gens sagbi(M, Limit => 17);
ans = matrix {{x^3+y^3+z^3, x^4+y^4+z^4, x^5+y^5+z^5,
    x^5*y^3-2*x^4*y^4+x^3*y^5+x^5*z^3+y^5*z^3-2*x^4*z^4-2*y^4*z^4+x^3*z^5+y^3*z^5,
    x^6*y^3-(1/3)*x^5*y^4-(1/3)*x^4*y^5+x^3*y^6+x^6*z^3+
    2*x^3*y^3*z^3+y^6*z^3-(1/3)*x^5*z^4-(1/3)*y^5*z^4-(1/3)*x^4*z^5-(1/3)*y^4*z^5+x^3*z^6+y^3*z^6,
    x^7*y^3+(1/2)*x^6*y^4-x^5*y^5+(1/2)*x^4*y^6+x^3*y^7+
    x^7*z^3+x^4*y^3*z^3+x^3*y^4*z^3+y^7*z^3+(1/2)*x^6*z^4+
    x^3*y^3*z^4+(1/2)*y^6*z^4-x^5*z^5-y^5*z^5+(1/2)*x^4*z^6+(1/2)*y^4*z^6+x^3*z^7+y^3*z^7,
    x^8*y^4-(4/5)*x^7*y^5+(6/5)*x^6*y^6-(4/5)*x^5*y^7+
    x^4*y^8+(12/5)*x^6*y^3*z^3-(4/5)*x^5*y^4*z^3-
    (4/5)*x^4*y^5*z^3+(12/5)*x^3*y^6*z^3+x^8*z^4-
    (4/5)*x^5*y^3*z^4+(18/5)*x^4*y^4*z^4-(4/5)*x^3*y^5*z^4+
    y^8*z^4-(4/5)*x^7*z^5-(4/5)*x^4*y^3*z^5-(4/5)*x^3*y^4*z^5-
    (4/5)*y^7*z^5+(6/5)*x^6*z^6+(12/5)*x^3*y^3*z^6+(6/5)*y^6*z^6-
    (4/5)*x^5*z^7-(4/5)*y^5*z^7+x^4*z^8+y^4*z^8,
    x^9*y^4-(4/5)*x^8*y^5+(3/5)*x^7*y^6+(3/5)*x^6*y^7-
    (4/5)*x^5*y^8+x^4*y^9+(6/5)*x^7*y^3*z^3+(3/5)*x^6*y^4*z^3-
    (6/5)*x^5*y^5*z^3+(3/5)*x^4*y^6*z^3+(6/5)*x^3*y^7*z^3+x^9*z^4+
    (3/5)*x^6*y^3*z^4+(4/5)*x^5*y^4*z^4+(4/5)*x^4*y^5*z^4+
    (3/5)*x^3*y^6*z^4+y^9*z^4-(4/5)*x^8*z^5-(6/5)*x^5*y^3*z^5+
    (4/5)*x^4*y^4*z^5-(6/5)*x^3*y^5*z^5-(4/5)*y^8*z^5+(3/5)*x^7*z^6+
    (3/5)*x^4*y^3*z^6+(3/5)*x^3*y^4*z^6+(3/5)*y^7*z^6+(3/5)*x^6*z^7+
    (6/5)*x^3*y^3*z^7+(3/5)*y^6*z^7-(4/5)*x^5*z^8-(4/5)*y^5*z^8+x^4*z^9+y^4*z^9,
    x^10*y^4-(4/5)*x^9*y^5-(3/5)*x^8*y^6+(12/5)*x^7*y^7-(3/5)*x^6*y^8-
    (4/5)*x^5*y^9+x^4*y^10-(6/5)*x^8*y^3*z^3+(12/5)*x^7*y^4*z^3-
    (6/5)*x^6*y^5*z^3-(6/5)*x^5*y^6*z^3+(12/5)*x^4*y^7*z^3-
    (6/5)*x^3*y^8*z^3+x^10*z^4+(12/5)*x^7*y^3*z^4+(6/5)*x^6*y^4*z^4-
    (2/5)*x^5*y^5*z^4+(6/5)*x^4*y^6*z^4+(12/5)*x^3*y^7*z^4+y^10*z^4-
    (4/5)*x^9*z^5-(6/5)*x^6*y^3*z^5-(2/5)*x^5*y^4*z^5-(2/5)*x^4*y^5*z^5-
    (6/5)*x^3*y^6*z^5-(4/5)*y^9*z^5-(3/5)*x^8*z^6-(6/5)*x^5*y^3*z^6+
    (6/5)*x^4*y^4*z^6-(6/5)*x^3*y^5*z^6-(3/5)*y^8*z^6+(12/5)*x^7*z^7+
    (12/5)*x^4*y^3*z^7+(12/5)*x^3*y^4*z^7+(12/5)*y^7*z^7-(3/5)*x^6*z^8-
    (6/5)*x^3*y^3*z^8-(3/5)*y^6*z^8-(4/5)*x^5*z^9-(4/5)*y^5*z^9+x^4*z^10+y^4*z^10,
    x^9*y^6-(3/7)*x^8*y^7-(3/7)*x^7*y^8+x^6*y^9+2*x^9*y^3*z^3-
    (3/7)*x^8*y^4*z^3-(6/7)*x^7*y^5*z^3+(30/7)*x^6*y^6*z^3-
    (6/7)*x^5*y^7*z^3-(3/7)*x^4*y^8*z^3+2*x^3*y^9*z^3-
    (3/7)*x^8*y^3*z^4+(6/7)*x^7*y^4*z^4-(3/7)*x^6*y^5*z^4-
    (3/7)*x^5*y^6*z^4+(6/7)*x^4*y^7*z^4-(3/7)*x^3*y^8*z^4-
    (6/7)*x^7*y^3*z^5-(3/7)*x^6*y^4*z^5+(6/7)*x^5*y^5*z^5-
    (3/7)*x^4*y^6*z^5-(6/7)*x^3*y^7*z^5+x^9*z^6+(30/7)*x^6*y^3*z^6-
    (3/7)*x^5*y^4*z^6-(3/7)*x^4*y^5*z^6+(30/7)*x^3*y^6*z^6+y^9*z^6-
    (3/7)*x^8*z^7-(6/7)*x^5*y^3*z^7+(6/7)*x^4*y^4*z^7-
    (6/7)*x^3*y^5*z^7-(3/7)*y^8*z^7-(3/7)*x^7*z^8-
    (3/7)*x^4*y^3*z^8-(3/7)*x^3*y^4*z^8-
    (3/7)*y^7*z^8+x^6*z^9+2*x^3*y^3*z^9+y^6*z^9}}
assert(
    partialSBSeventeen
    ==
    ans
    )
///

-- 28) Bruns/Conca 2
TEST ///
FF = ZZ/2
R = FF[x,y,z]
T = { x^6, x^5*y, y^5*z, z^5*x,    y^6+ y^3*z^3 }
G = gens sagbi(subring T, Limit=>95);
ans = matrix {{x*z^5, y^5*z, y^6+y^3*z^3, x^5*y, x^6, x^6*y^13*z^17,
    x^30*y^3*z^3, x^30*y^9*z^9, x^12*y^25*z^29, x^12*y^17*z^43,
    x^12*y^18*z^42+x^12*y^15*z^45, x^6*y^37*z^41, x^12*y^24*z^54,
    x^11*y^39*z^40+x^11*y^30*z^49+x^11*y^27*z^52}}
assert(
    G
    ==
    ans)
///

end --
