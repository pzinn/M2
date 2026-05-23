R = ZZ/101[x,y,z]
f = x^2
I = matrix{{x^2,y^2}}
P=matrix({{2,3,4}})
substitute(f,P)
substitute(I,P)

S = QQ[t,Inverses=>true,MonomialOrder=>Lex]
g = matrix{{t^-1,1/2}}
assert(try (sub(g,t=>0); false) else true)
assert(sub(matrix{{t,1/2}}, t=>0) == matrix{{0_QQ,1/2}})
assert(sub(t^-1, t=>2) == 1/2)

end
-- Local Variables:
-- compile-command: "make -C $M2BUILDDIR/Macaulay2/packages/Macaulay2Doc/test subst.out"
-- End:
