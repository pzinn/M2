-- input table of scalar products
scalar = matrix {{0,0,0,0,1,1,0,1,0,0,1,1,1,1,0,1,1,0,1,0,1,1,1,1,1,2,1},{1,0,0,0,0,1,1,1,1,0,0,0,0,1,1,1,1,1,0,0,1,1,2,1,1,1,1},{1,1,0,0,1,0,0,1,1,1,0,1,0,1,0,2,1,1,1,0,0,0,1,1,1,1,1},{1,1,1,0,1,1,1,0,0,0,1,0,0,1,0,1,1,1,1,0,0,1,1,1,2,1,0},{0,1,0,0,0,0,1,0,1,0,1,1,0,1,0,1,0,1,1,1,1,1,1,2,1,1,1},{0,1,1,0,1,0,0,0,0,1,1,1,1,0,0,1,1,1,2,1,0,1,0,1,1,1,1},{1,0,1,0,1,1,0,1,0,1,0,0,1,0,1,1,2,1,1,0,0,1,1,0,1,1,1},{0,1,1,1,1,1,1,0,0,0,2,1,1,1,0,0,0,0,1,1,1,1,0,1,1,1,0},{1,0,1,1,1,2,1,1,0,0,1,0,1,1,1,0,1,0,0,0,1,1,1,0,1,1,0},{1,1,0,1,1,1,1,1,1,0,1,1,0,2,0,1,0,0,0,0,1,0,1,1,1,1,0},{1,1,1,0,0,0,1,0,1,1,0,0,0,0,1,1,1,2,1,1,0,1,1,1,1,0,1},{1,1,1,1,0,1,2,0,1,0,1,0,0,1,1,0,0,1,0,1,1,1,1,1,1,0,0},{1,2,1,1,1,0,1,0,1,1,1,1,0,1,0,1,0,1,1,1,0,0,0,1,1,0,0},{0,0,1,0,0,1,1,0,0,0,1,0,1,0,1,0,1,1,1,1,1,2,1,1,1,1,1},{1,1,1,1,2,1,0,1,0,1,1,1,1,1,0,1,1,0,1,0,0,0,0,0,1,1,0},{0,0,0,1,0,1,1,1,1,0,1,1,1,1,1,0,0,0,0,1,2,1,1,1,0,1,1},{0,1,0,1,1,0,0,1,1,1,1,2,1,1,0,1,0,0,1,1,1,0,0,1,0,1,1},{1,0,0,1,1,1,0,2,1,1,0,1,1,1,1,1,1,0,0,0,1,0,1,0,0,1,1},{1,1,0,1,0,0,1,1,2,1,0,1,0,1,1,1,0,1,0,1,1,0,1,1,0,0,1},{2,1,1,1,1,1,1,1,1,1,0,0,0,1,1,1,1,1,0,0,0,0,1,0,1,0,0},{1,1,2,1,1,1,1,0,0,1,1,0,1,0,1,0,1,1,1,1,0,1,0,0,1,0,0},{1,1,1,1,1,0,0,1,1,2,0,1,1,0,1,1,1,1,1,1,0,0,0,0,0,0,1},{0,0,1,1,1,1,0,1,0,1,1,1,2,0,1,0,1,0,1,1,1,1,0,0,0,1,1},{1,0,1,1,0,1,1,1,1,1,0,0,1,0,2,0,1,1,0,1,1,1,1,0,0,0,1},{0,0,0,0,0,0,0,1,1,1,0,1,1,0,1,1,1,1,1,1,1,1,1,1,0,1,2},{0,1,1,1,0,0,1,0,1,1,1,1,1,0,1,0,0,1,1,2,1,1,0,1,0,0,1},{1,1,1,2,1,1,1,1,1,1,1,1,1,1,1,0,0,0,0,1,1,0,0,0,0,0,0}};
--ℏ:=getSymbol "ℏ";
--Rfug_0 = frac(factor(ZZ[ℏ])); -- init index table
--xbar:=getSymbol "xbar";
--Rfug1 = frac(factor(ZZ[ℏ,xbar]));
--ℏ=Rfug1_0; xbar=Rfug1_1;
ℏ:=FH1_0; xbar:=FH1_1;
-- H_T fugacity in terms of scalar products
fug = matrix { { 1,0,0 }, 
    {ℏ/(ℏ-xbar),xbar/(ℏ-xbar),0}, 
    {4*ℏ^2/(ℏ-xbar)/(4*ℏ-xbar),ℏ*xbar/(ℏ-xbar)/(4*ℏ-xbar),-xbar*(3*ℏ-xbar)/(ℏ-xbar)/(4*ℏ-xbar)}
    };
states3:=makeStates 3;
ind := x -> position(states3,y->y===x);
fugacityH = p -> (
        n:=p.Size; x:=getSymbol "x"; ℏ:=getSymbol "ℏ";
        if not FH#?n then FH_n = frac(factor(ZZ[ℏ,x_1..x_n]));
	FF = FH_n;
        product(n-1, i -> product(n-1-i, j -> (
		    X := p#(i,j,1); W:=p#(i,j,0); U := p#(i+1,j,0);
                    if not p#Separated then (
                    	X = ind X; W = ind W; U = ind U;
			s := scalar_(U,X);
			t := scalar_(W,X); -- print(i,j,X,W,U,s,t);
			) else (
			if X == W then ( s=1; t=1; ) else if X == U then (  s=1; t=0; ) else ( s=0; t=0; ); -- A_n scalar products ~ A_1 scalar products
			);
                    (map(FF,frac FH1,{FF_0,FF_(n-i)-FF_(j+1)})) fug_(s,t)
                    ))))
-- K_T fugacity
q:=FK1_0; zbar:=FK1_1;
fugK = 1/(1-q^2*zbar) * matrix {{1 - q^2*zbar, 0, 0, 0, 0, 0, 0, -((-1 + q)*(1 + q)*zbar), 0}, {0, 0, 0, 1 - q^2*zbar, 0, 0, 0, 0, 
  ((-1 + q)*(1 + q)*zbar)/q}, {0, 0, 0, 0, 0, 0, -(q*(-1 + zbar)), 0, 0}, {0, -(q*(-1 + zbar)), 0, 0, 0, 0, 0, 0, 0}, 
 {0, 0, -((-1 + q)*(1 + q)*zbar), 0, 1 - q^2*zbar, 0, 0, 0, 0}, {-((-1 + q)*(1 + q)), 0, 0, 0, 0, 0, 0, 1 - q^2*zbar, 0}, 
 {0, 0, 1 - q^2*zbar, 0, -((-1 + q)*(1 + q)), 0, 0, 0, 0}, {0, 0, 0, 0, 0, -(q*(-1 + zbar)), 0, 0, 0}, 
 {0, 0, 0, (-1 + q)*q*(1 + q), 0, 0, 0, 0, 1 - q^2*zbar}};
states1:=makeStates 1;
ind1 := x -> position(states1,y->y===x);
fugacityK = p -> (
    if p.Steps>1 then error "K-fugacities at d=1 only";  -- for now, only d=1
    n:=p.Size; z:=getSymbol "z"; q:=getSymbol "q";
    if not FK#?n then FK_n = frac(factor(ZZ[q,z_1..z_n]));
    FF = FK_n;
    product(n-1, i -> product(n-1-i, j -> (
		X := p#(i,j,1); W:=p#(i,j,0); U := p#(i+1,j,0); V := p#(i,j+1,1);
		X = ind1 X; W = ind1 W; U = ind1 U; V = ind1 V;
		(map(FF,frac FK1,{FF_0,FF_(n-i)/FF_(j+1)})) fugK_(U*3+V,X*3+W)
		)))
    )
fugacity = p -> if class FF === FractionField and toString FF_0 === "q" then fugacityK p else fugacityH p
-- primitive: read options of puzzles instead? but Generic doesn't distinguish...

bottom = p -> (
    L := apply(p.Size,i->p#(p.Size-1-i,i,2));
    new AryString from apply(L, x -> if #x === 1 then value x else x)
    )

--tallyFugacities = L -> applyKeys(hashTable apply(L,p->p=>fugacity p),bottom,plus)
fugacityTally = L -> sum(L,p->new VirtualTally from {bottom p=>fugacity p})

fugacityVector = L -> (
    if #L === 0 then error "can't determine puzzle size";
    I := unique permutations sort bottom(first L);
    t := fugacityTally L;
    vector apply(I,i->t_i)
    )
