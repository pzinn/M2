-- H_T first
-- input table of scalar products d<=3
scalar = matrix {{0,0,0,0,1,1,0,1,0,0,1,1,1,1,0,1,1,0,1,0,1,1,1,1,1,2,1},{1,0,0,0,0,1,1,1,1,0,0,0,0,1,1,1,1,1,0,0,1,1,2,1,1,1,1},{1,1,0,0,1,0,0,1,1,1,0,1,0,1,0,2,1,1,1,0,0,0,1,1,1,1,1},{1,1,1,0,1,1,1,0,0,0,1,0,0,1,0,1,1,1,1,0,0,1,1,1,2,1,0},{0,1,0,0,0,0,1,0,1,0,1,1,0,1,0,1,0,1,1,1,1,1,1,2,1,1,1},{0,1,1,0,1,0,0,0,0,1,1,1,1,0,0,1,1,1,2,1,0,1,0,1,1,1,1},{1,0,1,0,1,1,0,1,0,1,0,0,1,0,1,1,2,1,1,0,0,1,1,0,1,1,1},{0,1,1,1,1,1,1,0,0,0,2,1,1,1,0,0,0,0,1,1,1,1,0,1,1,1,0},{1,0,1,1,1,2,1,1,0,0,1,0,1,1,1,0,1,0,0,0,1,1,1,0,1,1,0},{1,1,0,1,1,1,1,1,1,0,1,1,0,2,0,1,0,0,0,0,1,0,1,1,1,1,0},{1,1,1,0,0,0,1,0,1,1,0,0,0,0,1,1,1,2,1,1,0,1,1,1,1,0,1},{1,1,1,1,0,1,2,0,1,0,1,0,0,1,1,0,0,1,0,1,1,1,1,1,1,0,0},{1,2,1,1,1,0,1,0,1,1,1,1,0,1,0,1,0,1,1,1,0,0,0,1,1,0,0},{0,0,1,0,0,1,1,0,0,0,1,0,1,0,1,0,1,1,1,1,1,2,1,1,1,1,1},{1,1,1,1,2,1,0,1,0,1,1,1,1,1,0,1,1,0,1,0,0,0,0,0,1,1,0},{0,0,0,1,0,1,1,1,1,0,1,1,1,1,1,0,0,0,0,1,2,1,1,1,0,1,1},{0,1,0,1,1,0,0,1,1,1,1,2,1,1,0,1,0,0,1,1,1,0,0,1,0,1,1},{1,0,0,1,1,1,0,2,1,1,0,1,1,1,1,1,1,0,0,0,1,0,1,0,0,1,1},{1,1,0,1,0,0,1,1,2,1,0,1,0,1,1,1,0,1,0,1,1,0,1,1,0,0,1},{2,1,1,1,1,1,1,1,1,1,0,0,0,1,1,1,1,1,0,0,0,0,1,0,1,0,0},{1,1,2,1,1,1,1,0,0,1,1,0,1,0,1,0,1,1,1,1,0,1,0,0,1,0,0},{1,1,1,1,1,0,0,1,1,2,0,1,1,0,1,1,1,1,1,1,0,0,0,0,0,0,1},{0,0,1,1,1,1,0,1,0,1,1,1,2,0,1,0,1,0,1,1,1,1,0,0,0,1,1},{1,0,1,1,0,1,1,1,1,1,0,0,1,0,2,0,1,1,0,1,1,1,1,0,0,0,1},{0,0,0,0,0,0,0,1,1,1,0,1,1,0,1,1,1,1,1,1,1,1,1,1,0,1,2},{0,1,1,1,0,0,1,0,1,1,1,1,1,0,1,0,0,1,1,2,1,1,0,1,0,0,1},{1,1,1,2,1,1,1,1,1,1,1,1,1,1,1,0,0,0,0,1,1,0,0,0,0,0,0}};
ℏ:=FH_0_0; xbar:=FH_-1_1;
-- H_T fugacity in terms of scalar products d<=3
fug = matrix { { 1,0,0 },
    {ℏ/(ℏ-xbar),xbar/(ℏ-xbar),0},
    {4*ℏ^2/(ℏ-xbar)/(4*ℏ-xbar),ℏ*xbar/(ℏ-xbar)/(4*ℏ-xbar),-xbar*(3*ℏ-xbar)/(ℏ-xbar)/(4*ℏ-xbar)}
    };
fugacityH = p -> ( -- equivariant H
    states3:=makeStates 3;
    ind := x -> position(states3,y->y===x);
    n:=p.Size;
    defineFH n;
    product(n-1, i -> product(n-1-i, j -> (
                X := p#(i,j,1); W:=p#(i,j,0); U := p#(i+1,j,0);
                if not p#?Separation then (
                    X = ind X; W = ind W; U = ind U;
                    s := scalar_(U,X);
                    t := scalar_(W,X); -- print(i,j,X,W,U,s,t);
                    ) else (
                    if X == W then ( s=1; t=1; ) else if X == U then (  s=1; t=0; ) else ( s=0; t=0; ); -- A_n scalar products ~ A_1 scalar products
                    );
                (map(FH_n,FH_-1,{FH_n_0,FH_n_(n-i)-FH_n_(j+1)})) fug_(s,t)
                ))))

q:=FK_0_0; zbar:=FK_-1_1;
--
fugacityK = p -> (
    d:=p.Steps;
    n:=p.Size;
    if p#?Separation then (
	tri := (a,b) -> if  a==" " or b==" " or a<b then 1
			else if a>=p#Separation and b<p#Separation then -q^(-1) else -q; -- probably wrong needs more checks
	if p#Equivariant then (
	    defineFK n;
            product(n-1, i -> product(n-1-i, j ->
		    (
			z := FK_n_(n-i)/FK_n_(j+1);
			(a,b,c,d) := (p#(i+1,j,0),p#(i,j+1,1),p#(i,j,1),p#(i,j,0)); -- i,j,k,l
			if a==b then (if a==" " then q else 1)*(1-z)/(1-q^2*z) else if a==d then 1
			else ((1-q^2)/(1-q^2*z)
			    * (if a>b or a==" " or b==" " then 1 else z) -- probably wrong
			    * (tri(a,b))^(-1) * tri(d,c)
			    )
			)
                    )) * product(n,i->(
                    tri(p#(i,n-1-i,0),p#(i,n-1-i,1))
                    )
		)
	    ) else (
            product(n, i -> product(n-i, j -> tri(p#(i,j,0),p#(i,j,1))
		    * (if j+i==n-1 then 1 else (tri(p#(i+1,j,0),p#(i,j+1,1)))^(-1)
            )))
	)
    ) else if p#Equivariant then (
        (uptrifug,downtrifug) := try (myget ("fugacity-"|toString d|".m2"))(q) else error "K-fugacities not implemented for this value of d";
        --(uptrifug,downtrifug) := myget ("fugacity-"|toString d|".m2");
        rhfug := try (myget ("fugacity-equiv-"|toString d|".m2"))(q,zbar) else error "K-fugacities not implemented for this value of d";
	defineFK n;
        product(n-1, i -> product(n-1-i, j ->
                (map(FK_n,FK_-1,{FK_n_0,FK_n_(n-i)/FK_n_(j+1)})) rhfug#(p#(i+1,j,0),p#(i,j+1,1),p#(i,j,1),p#(i,j,0))
                )) * product(n,i->(
                uptrifug#(p#(i,n-1-i,0),p#(i,n-1-i,1),p#(i,n-1-i,2))
                )
            )
        ) else (
        (uptrifug,downtrifug) = try (myget ("fugacity-"|toString d|".m2"))(q) else error "K-fugacities not implemented for this value of d";
        product(n, i -> product(n-i, j ->
                uptrifug#(p#(i,j,0),p#(i,j,1),p#(i,j,2))
		* if j+i==n-1 then 1 else downtrifug#(p#(i+1,j,0),p#(i,j+1,1),p#(i,j,2))))
        )
    )

fugacity = true >> o -> p -> (
    if #o>0 then p = p ++ o; -- change options
    if not p#Generic then error "Non generic fugacities not implemented yet";
    if not p#?Separation and p#Steps > 3 then error "Fugacities not implemented yet for d>3";
    if not p#Equivariant and not p#Kth then return 1; -- ha
    (if p#Kth then fugacityK else fugacityH) p
    )

bottom = p -> (
    L := apply(p.Size,i->p#(p.Size-1-i,i,2));
    new AryString from apply(L, x -> if #x === 1 then value x else x)
    )

--tallyFugacities = true >> o -> L -> applyKeys(hashTable apply(L,p->p=>fugacity p),bottom,plus)
fugacityTally = true >> o -> L -> sum(L,p->new VirtualTally from {bottom p=>fugacity(p,o)})

String ? ZZ := (s,n) -> s ? concatenate(#s:toString n) -- 0 < 10 < 1 < 21 < 2 ...
ZZ ? String := (n,s) -> concatenate(#s:toString n) ? s


fugacityVector = true >> o -> L -> (
    if #L === 0 then return 0; -- error "can't determine puzzle size";
    I := unique permutations sort bottom(first L);
    t := fugacityTally(L,o);
    vector apply(I,i->t_i)
    )

end

needsPackage "CotangentSchubert"
(FF,I)=setupCotangent(1,2,3,Kth=>true)
segreCls = segreClasses();
T=table(I,I,(i,j)->segreCls^(-1)*(segreClass i @ segreClass j));
TT=table(I,I,(i,j)->fugacityVector puzzle(i,j,Generic=>true,Equivariant=>true,Kth=>true));
T==TT

(FF,I)=setupCotangent(1,2,3,4,Kth=>true)
segreCls = segreClasses();
segreInv = segreCls^(-1);
