--		Copyright 2009 by Daniel R. Grayson
-- rewritten by P. Zinn-Justin 2021

needs "code.m2"
needs "indeterminates.m2"
needs "ofcm.m2"
needs "variables.m2"

scan({symbol ..,symbol ..<},{(x,y)->x..y,(x,y)->x..<y},(sym,fun)->(
	installMethod(sym,RingElement,RingElement, (x,y) -> (
    		if class x =!= class y then try x = promote(x,class y) else try y=promote(y,class x);
    		R := class x;
    		i := index x;
    		j := index y;
    		if R === class y and instance(i,ZZ) and instance(j,ZZ) then apply(fun(i,j),k->R_k) else fun(baseName x,baseName y)
		));
	installMethod(sym,Thing,Thing, (x,y) -> fun(baseName x,baseName y))
    ))

-- Local Variables:
-- compile-command: "make -C $M2BUILDDIR/Macaulay2/m2 "
-- End:
