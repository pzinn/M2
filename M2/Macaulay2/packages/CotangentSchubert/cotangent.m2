export {
    "setupCotangent",
    "tautClass",
    "sClass", "stableClass", "segreClass", "chernClass", "schubertClass",
    "sClass'", "stableClass'", "segreClass'", "chernClass'",
    "restrict", "fullToPartial", "basisCoeffs",
    "pushforwardToPoint", "pushforwardToPointFromCotangent", "zeroSection", "dualZeroSection",
    "Presentation", "Borel", "EquivLoc",
    "inversion",
    "Partial"
    };

cotOpts := opts ++ { Presentation => EquivLoc }

debug Core -- to use basering, generatorSymbols, frame

-- labeling of classes
AryString = new Type of List;
new AryString from String := (T,s) -> apply(ascii s,i->i-48);
texMath AryString := s -> concatenate between("\\,",apply(s,x -> if class x === String then x else texMath x))
net AryString := toString AryString := s -> concatenate apply(s,toString)
-- inversion number of a string
inversion = method()
inversion AryString := p -> sum(#p-1,i->sum(i+1..#p-1,j->if p_i>p_j then 1 else 0))
inversion String := s -> inversion new AryString from s

-- a simple function that seems like it should already exist
basisCoeffs = x -> lift(last coefficients(x, Monomials => basis ring x),(ring x).basering)

-- next two functions should be memoized
elem := (i,vrs) -> sum(subsets(vrs,i), product);
expandElem := (P,vrs,els) -> (
    if P == 0 then return 0;
    c := coefficients(P,Variables=>vrs);
    M := c#0_(0,0); C := c#1_(0,0);
    e := append((first exponents M)_(apply(vrs,index)),0);
    ee := apply(#vrs, i -> e#i - e#(i+1));
    if any(ee, i->i<0) then error "nonsymmetric polynomial";
    Q := P - C * product(#vrs, i -> (elem(i+1,vrs))^(ee#i));
    sub(C,ring first els) * product(#vrs, i -> (els#i)^(ee#i)) + expandElem(Q,vrs,els)
    )

-- automate promotion
promoteFromMap = method()
promoteFromMap (Ring,Ring,RingMap) := (R,S,f) -> (
    promote(R,S) := (a,S1) -> f a;
    promote(Matrix,R,S) :=
    promote(MutableMatrix,R,S) := -- doesn't work, cf https://github.com/Macaulay2/M2/issues/2192
    promote(Module,R,S) := (M,R1,S1) -> f M;
--    promote(List,R,S) := (L,R1,S1) -> f\L; -- TODO put back!!!!!!!!!!
    S.baseRings = prepend(R,S.baseRings); -- temporary -- until promotability test improved in enginering.m2
    )
promoteFromMap (Ring,Ring) := (R,S) -> promoteFromMap(R,S,map(S,R))

-- common rings
q := getSymbol "q"; zbar := getSymbol "zbar";
FK_-1 = frac(factor(ZZ (monoid[q,zbar,DegreeRank=>0]))); -- same as FK_1, really but diff variable name
FK_0 = frac(factor(ZZ (monoid[q,DegreeRank=>0])));
promoteFromMap(FK_0,FK_-1);

h := getSymbol "h"; xbar := getSymbol "xbar";
FH_-1 = frac(factor(ZZ (monoid[h,xbar]))); -- same as FH_1, really but diff variable name
FH_0 = frac(factor(ZZ (monoid[h])));
promoteFromMap(FH_0,FH_-1);

defineFK = n -> (
    if not FK#?n then (
        z := getSymbol "z"; -- q := getSymbol "q";
        FK_n = frac(factor(ZZ (monoid[q,z_1..z_n,DegreeRank=>0])));
        promoteFromMap(FK_0,FK_n);
        );
    FK#n
    )

defineFH = n -> (
    if not FH#?n then (
        x := getSymbol "x"; -- h := getSymbol "h";
        FH_n = frac(factor(ZZ (monoid[h,x_1..x_n])));
        promoteFromMap(FH_0,FH_n);
        );
    FH#n
    )

protect lastSetup;
lastSetup:=null;

BBs = new IndexedVariableTable;

defineB = (FF,n,Kth,Equivariant) -> ( -- TODO remove FF
    if not BBs#?(n,Kth,Equivariant) then (
	y := getSymbol "y";
	BB0 := FF(monoid(splice[y_1..y_n, if Kth then DegreeRank=>0 else (MonomialOrder=>{Weights=>{n:1},RevLex},DegreeRank=>1)])); -- in terms of Chern roots
	J := ideal apply(1..n,k->elem(k,gens BB0)
            -if Equivariant then elem(k,drop(gens FF,1)) else if Kth then binomial(n,k) else 0);
	BB := BB0/J;
	BBs#(n,Kth,Equivariant) = BB;
	-- a bunch of not quite right defs TODO improve
	tautClass (ZZ,ZZ,BB) := o -> (j,i,BB) -> tautClass(j,i,BB.cache.lastSetup,Partial=>false);
	zeroSection BB := o -> BB -> zeroSection(BB.cache.lastSetup,Partial=>false); -- not quite right but close enough for now
	dualZeroSection BB := o -> BB -> dualZeroSection(BB.cache.lastSetup,Partial=>false); -- not quite right but close enough for now
	zeroSectionInv BB := o -> BB -> zeroSectionInv(BB.cache.lastSetup,Partial=>false); -- not quite right but close enough for now
	sClass (List,BB) :=
	sClass (String,BB) :=
	sClass (AryString,BB) := o -> (i,BB) -> sClass(i,BB.cache.lastSetup,Partial=>false);
	segreClass (String,BB) :=
	segreClass (AryString,BB) := o -> (i,BB) -> segreClass(i,BB.cache.lastSetup,Partial=>false);
	stableClass (List,BB) :=
	stableClass (String,BB) :=
	stableClass (AryString,BB) := o -> (i,BB) -> stableClass(i,BB.cache.lastSetup,Partial=>false);
	chernClass (List,BB) :=
	chernClass (String,BB) :=
	chernClass (AryString,BB) := o -> (i,BB) -> chernClass(i,BB.cache.lastSetup,Partial=>false);
	schubertClass (List,BB) :=
	schubertClass (String,BB) :=
	schubertClass (AryString,BB) := o -> (i,BB) -> schubertClass(i,BB.cache.lastSetup,Partial=>false);
	sClass' (List,BB) :=
	sClass' (String,BB) :=
	sClass' (AryString,BB) := o -> (i,BB) -> sClass'(i,BB.cache.lastSetup,Partial=>false);
	segreClass' (List,BB) :=
	segreClass' (String,BB) :=
	segreClass' (AryString,BB) := o -> (i,BB) -> segreClass'(i,BB.cache.lastSetup,Partial=>false);
	chernClass' (List,BB) :=
	chernClass' (String,BB) :=
	chernClass' (AryString,BB) := o -> (i,BB) -> chernClass'(i,BB.cache.lastSetup,Partial=>false);
	stableClass' (List,BB) := o -> (L,BB) -> matrix { apply(L,i->stableClass'(i,BB.cache.lastSetup,Partial=>false)) };
	stableClass' (String,BB) :=
	stableClass' (AryString,BB) := o -> (i,BB) -> stableClass'(i,BB.cache.lastSetup,Partial=>false);
	);
    BBs#(n,Kth,Equivariant)
    )

-- diagonal algebra
DiagonalAlgebra = new Type of Type;
new DiagonalAlgebra from Module := (X,M) -> (
    D := new DiagonalAlgebra of Vector from hashTable { global Module => M };
    new D from List := (D,l) -> new D from vector l;
    new D from Vector := (D,v) -> (
	if class v =!= M then try (
	    v = promote(v,ring M);
	    assert(class v === M);
	    ) else error "wrong type of vector";
	v);
    new D from Number :=
    new D from RingElement := (D,x) -> new D from apply(rank M,i->x);
    vector D := d -> new M from d;
    matrix D := opts -> d -> diagonalMatrix entries d;
    D * Vector := (x,v) -> if class v === M then matrix x * v else error "wrong vector";
    D * Matrix := (x,m) -> if target m === M then matrix x * m else error "wrong target";
    D * D := (v,w) -> new D from apply(entries v,entries w,(x,y)->x*y); -- componentwise product
    D ^ ZZ := (v,n) -> new D from apply(entries v, a -> a^n); -- componentwise power
    D + Number := D + RingElement := (v,x) -> v + new D from x;
    Number + D := RingElement + D := (x,v) -> v + new D from x;
    D - Number := D - RingElement := (v,x) -> v - new D from x;
    Number - D := RingElement - D := (x,v) -> - v + new D from x;
    D == Vector := Vector == D := (x,y) -> x#0 == y#0;
    D == Number := (x,n) -> x == new D from n;
    Number == DD := (n,x) -> x == new D from n;
    D)
ring DiagonalAlgebra := D -> ring D.Module;
rank DiagonalAlgebra := D -> rank D.Module;
expression DiagonalAlgebra := D -> if hasAttribute(D,ReverseDictionary) then return expression getAttribute(D,ReverseDictionary) else (expression DiagonalAlgebra) D.Module;
net DiagonalAlgebra := D -> net expression D;
toString DiagonalAlgebra := D -> toString expression D;
texMath DiagonalAlgebra := D -> texMath expression D;
html DiagonalAlgebra := lookup(html,Thing);

-- ex: D=new DiagonalAlgebra from ZZ^3; x=new D from {1,2,3}; x^2-x+1

addLastSetup = f -> installMethod(f, o -> () -> if lastSetup =!= null then f(lastSetup,Partial => o.Partial =!= null and o.Partial) else error "Set up first");
addLastSetup1 = f -> f Thing := o -> x -> if lastSetup =!= null then f(x,lastSetup,Partial => o.Partial =!= null and o.Partial) else error "Set up first";
addLastSetup2 = f -> f (Thing,Thing) := o -> (x,y) -> if lastSetup =!= null then f(x,y,lastSetup,Partial => o.Partial =!= null and o.Partial) else error "Set up first";

zeroSection = method(Dispatch=>{Type},Options=>{Partial=>null}) -- note the {}
addLastSetup(zeroSection);

dualZeroSection = method(Dispatch=>{Type},Options=>{Partial=>null}) -- note the {}
addLastSetup(dualZeroSection);

zeroSectionInv = method(Dispatch=>{Type},Options=>{Partial=>null})
--addLastSetup(zeroSectionInv);

segreClass = method(Dispatch=>{Thing,Type},Options=>{Partial=>null})
addLastSetup1(segreClass);

sClass = method(Dispatch=>{Thing,Type},Options=>{Partial=>null})
addLastSetup1(sClass);

stableClass = method(Dispatch=>{Thing,Type},Options=>{Partial=>null})
addLastSetup1(stableClass);

chernClass = method(Dispatch=>{Thing,Type},Options=>{Partial=>null})
addLastSetup1(chernClass);

schubertClass = method(Dispatch=>{Thing,Type},Options=>{Partial=>null})
addLastSetup1(schubertClass);

sClass' = method(Dispatch=>{Thing,Type},Options=>{Partial=>null})
addLastSetup1(sClass');

stableClass' = method(Dispatch=>{Thing,Type},Options=>{Partial=>null})
addLastSetup1(stableClass');

segreClass' = method(Dispatch=>{Thing,Type},Options=>{Partial=>null})
addLastSetup1(segreClass');

chernClass' = method(Dispatch=>{Thing,Type},Options=>{Partial=>null})
addLastSetup1(chernClass');

sClasses = method(Dispatch=>{Type},Options=>{Partial=>null})

schubertClasses = method(Dispatch=>{Type},Options=>{Partial=>null})

-- "Chern classes" -- renamed tautClass to avoid confusion with motivic classes
tautClass = method(Dispatch=>{Thing,Thing,Type},Options=>{Partial=>null});
addLastSetup2(tautClass);

-- for internal use
weights = method(Dispatch=>{Type})
cotweights = method(Dispatch=>{Type})

-- the methods below are defined for appropriate rings by setup
-- the defs below are just placeholders (try to promote to latest) or apply-type
-- restriction to fixed points
--addLastSetup1n = f -> f Thing := x -> if lastSetup =!= null then f(x,lastSetup) else error "Set up first"; -- same as addLastSetup but no option <sigh>

restrict = method(Dispatch => {Thing,Type})
restrict Thing := x -> if (ring x).cache.?lastSetup then restrict(x,(ring x).cache.lastSetup) else restrict(x, ring x);
restrict (Matrix,RingElement) := (m,X) -> matrix apply(flatten entries m,x->restrict(x,X)) -- only for one-row matrices

-- from full flag to partial flag
fullToPartial = method(Dispatch =>{Thing,Type})
fullToPartial Thing := x -> if (ring x).cache.?lastSetup then fullToPartial(x,(ring x).cache.lastSetup) else fullToPartial(x, ring x);
fullToPartial (Matrix,RingElement) := (m,X) -> matrix applyTable(entries m,x->fullToPartial(x,X))

-- pushforward TODO rewrite as restrict and ftop
pushforwardToPoint=method(); -- pushforward to a point from K(G/P)
pushforwardToPoint Number := pushforwardToPoint RingElement := r -> try pushforwardToPoint promote(r,lastSetup) else error "can't pushforward"; -- ?
pushforwardToPoint Matrix := m -> (
    if (ring m)#?pushforwardToPoint then matrix applyTable(entries m,pushforwardToPoint)
    else try weights lastSetup * m
    else error "can't pushforward"
    )

pushforwardToPointFromCotangent=method(); -- pushforward to a point from K(T^*(G/P))
pushforwardToPointFromCotangent Number := pushforwardToPointFromCotangent RingElement := r -> try pushforwardToPointFromCotangent promote(r,lastSetup) else error "can't pushforward"; -- ?
pushforwardToPointFromCotangent Matrix := m -> (
    if (ring m)#?pushforwardToPointFromCotangent then matrix applyTable(entries m,pushforwardToPointFromCotangent)
    else try cotweights lastSetup * m
    else error "can't pushforward"
    )


-- main function: set up everything
setupCotangent = cotOpts >> curCotOpts -> dims0 -> (
    if #dims0 === 0 or unique dims0 === {0} then error "Please specify nonzero dimensions";
    -- "global" parameters
    dims := if first dims0 == 0 then dims0 else prepend(0,dims0);
    n := last dims;
    d := #dims - 2; -- # steps - 2 since includes trivial first and last
    subs := s -> apply(d+1,i->positions(s,j->j==i));
    dimdiffs := apply(d+1, i-> dims#(i+1)-dims#i);
    ω:=new AryString from splice apply(d+1, i->dimdiffs_i:i); -- list of fixed points
    I := unique permutations ω; -- unique? eww
    ind := method();
    ind AryString := i -> sum(#i,j->(d+1)^j*i_(#i-1-j));
    ind String := s -> ind new AryString from s;
    -- redefine default puzzle opts
    (frame puzzle)#0 = applyPairs((frame puzzle)#0,(k,v) -> (k,if curCotOpts#?k then curCotOpts#k else v)); -- TODO use Factor's new "fuse"
    -- set up base ring and R-matrices
    if curCotOpts.Kth then (
	FF0 := FK_0;
	FF := if curCotOpts.Equivariant then defineFK n else FF0;
	V1:=FK_-1^(d+1); q:=FK_-1_0; zbar:=FK_-1_1;
	Rcnum0:=map(V1^**2,V1^**2,splice flatten table(d+1,d+1,(i,j)->
		if i==j then (i*(d+2),i*(d+2))=>1-q^2*zbar
		else ((i*(d+1)+j,j*(d+1)+i)=>q*(1-zbar),
                    (i*(d+1)+j,i*(d+1)+j)=>(1-q^2)* if i<j then 1 else zbar)));
	Rcz0:=map(V1^**2,V1^**2,splice flatten table(d+1,d+1,(i,j)->
		if i==j then (i*(d+2),i*(d+2))=>1
		else ((i*(d+1)+j,j*(d+1)+i)=>if i<j then 1-zbar^(-1) else 0,
                    (i*(d+1)+j,i*(d+1)+j)=>if i<j then 1 else zbar^(-1)))); -- note the annoying ^(-1)
	Rcden0:=1-q^2*zbar;
	Rc0 := 1/Rcden0 * Rcnum0;
	-- TODO rewrite next 4 statements better. also do we need qq???
	Rc := (qq,z1,z2) -> (map(ring z2,FK_-1,{qq,z2/z1}))Rc0;
	Rcnum := (qq,z1,z2) -> (map(ring z2,FK_-1,{qq,z2*z1^(-1)}))Rcnum0;
	Rcden := (qq,z1,z2) -> (map(ring z2,FK_-1,{qq,z2*z1^(-1)}))Rcden0;
	Rcz := (qq,z1,z2) -> (map(ring z2,FK_-1,{qq,z2*z1^(-1)}))Rcz0;
	) else (
	FF0 = FH_0;
	FF = if curCotOpts.Equivariant then defineFH n else FF0;
	V1=FH_-1^(d+1); h:=FH_-1_0; xbar:=FH_-1_1;
	Rcnum0=map(V1^**2,V1^**2,splice flatten table(d+1,d+1,(i,j)->
		if i==j then (i*(d+2),i*(d+2))=>h-xbar
		else ((i*(d+1)+j,j*(d+1)+i)=>xbar,
                    (i*(d+1)+j,i*(d+1)+j)=>h)));
	Rcz0=map(V1^**2,V1^**2,splice flatten table(d+1,d+1,(i,j)->
		if i==j then (i*(d+2),i*(d+2))=>1
		else ((i*(d+1)+j,j*(d+1)+i)=>if i<j then xbar else 0,
                    (i*(d+1)+j,i*(d+1)+j)=>1)));
	Rcden0=h-xbar;
	Rc0 = 1/Rcden0 * Rcnum0;
	-- TODO rewrite next 4 statements better. also do we need hh???
	Rc = (hh,x1,x2) -> (map(ring x2,FH_-1,{hh,x2-x1}))Rc0;
	Rcnum = (hh,x1,x2) -> (map(ring x2,FH_-1,{hh,x2-x1}))Rcnum0;
	Rcden = (hh,x1,x2) -> (map(ring x2,FH_-1,{hh,x2-x1}))Rcden0;
	Rcz = (hh,x1,x2) -> (map(ring x2,FH_-1,{hh,x2-x1}))Rcz0;
        );
    if curCotOpts.Presentation === Borel then (
	BB := defineB(FF,n,curCotOpts.Kth,curCotOpts.Equivariant);
	if curCotOpts.Equivariant then promoteFromMap(FF0,BB,map(BB,FF0,{FF_0})); -- TODO move elsewhere
	y := getSymbol "y";
	-- Chern classes
	inds := splice apply(d+1, i -> apply(1..dimdiffs#i,j->(j,i)));
	v := (j,i) -> y_(j,toList(dims#i+1..dims#(i+1))); -- variable name
	e := (j,i) -> elem(j,apply(dims#i..dims#(i+1)-1,k->BB_k)); -- expression in terms of Chern roots
	args := v\inds;
	if curCotOpts.Kth then args = append(args,DegreeRank=>0) else (
	    degs := splice apply(d+1,i->1..dimdiffs#i);
	    args = append(args, Degrees=>degs);
	    wgts := apply(splice apply(d+1,i->reverse(dims#i..dims#(i+1)-1)),i->Weights=>apply(#inds,j->if j==i then -1 else 0));
	    args = append(args, MonomialOrder=>prepend(Weights=>degs,wgts)); -- a sort of GRevLex but with different ordering of variables
	    );
	R1 := FF monoid new Array from args;
	f := map(BB,R1,e\inds);
	AA := R1 / kernel f;
	lastSetup=BB.cache.lastSetup=AA;
	if curCotOpts.Equivariant then promoteFromMap(FF0,AA,map(AA,FF0,{FF_0}));
	tautClass (ZZ,ZZ,AA) := o -> (j,i,AA) -> if o.Partial === null or o.Partial then AA_(dims#i+j-1) else e (j,i);
	promoteFromMap(AA,BB,f*map(R1,AA));
	-- reverse transformation
	fullToPartial (Number,AA) :=
	fullToPartial (FF,AA) :=
	fullToPartial (BB,AA) := (b,AA) -> (
	    if d == n-1 then return (map(AA,BB,gens AA)) b; -- special case of full flag
	    AB := FF monoid (BB.generatorSymbols | AA.generatorSymbols); -- no using it
	    b = sub(b,AB);
	    -- scan(d+1,i->b=expandElem(b,toList(AB_(dims#i)..AB_(dims#(i+1)-1)),toList(AB_(n+dims#i)..AB_(n+dims#(i+1)-1))));
	    -- fails because of https://github.com/Macaulay2/M2/issues/2020
	    v := seq -> apply(toList seq, j -> AB_j);
	    scan(d+1,i->b=expandElem(b,v(dims#i..dims#(i+1)-1),v(n+dims#i..n+dims#(i+1)-1)));
	    sub(b,AA)
	    );
	zeroSection AA := o -> (
	    prt := o.Partial === null or o.Partial;
	    (cacheValue (zeroSection,prt)) (if prt then AA -> fullToPartial zeroSection(AA,Partial=>false)
	    else if curCotOpts.Kth then
	    AA -> product(n,j->product(n,k->if ω#j<ω#k then 1-FF_0^2*BB_j*BB_k^(-1) else 1))
	    else AA -> product(n,j->product(n,k->if ω#j<ω#k then FF_0-BB_j+BB_k else 1))));
	dualZeroSection AA := o -> (
	    prt := o.Partial === null or o.Partial;
	    (cacheValue (dualZeroSection,prt)) (if prt then AA -> fullToPartial dualZeroSection(AA,Partial=>false)
	    else if curCotOpts.Kth then
	    AA -> product(n,j->product(n,k->if ω#j<ω#k then 1-FF_0^-2*BB_k*BB_j^(-1) else 1))
	    else AA -> product(n,j->product(n,k->if ω#j<ω#k then -FF_0+BB_j-BB_k else 1))));
	zeroSectionInv AA := o -> (cacheValue (zeroSectionInv,o.Partial)) (AA -> (zeroSection(AA,o))^(-1));
	-- segre Classes
	sClasses AA := o -> (
	    prt := o.Partial === null or o.Partial;
	    (cacheValue (sClasses,prt)) (if prt then AA -> fullToPartial sClasses(AA,Partial=>false)
		else AA -> (
		-- monodromy matrix
		V:=BB^(d+1);
		W:=V^**n;
		Z:=map(BB^1,W,{{rank W-1:0,1}});
		scan(reverse(0..dims#d-1),i->( -- not 0..n-1: slight optimization: don't do trivial rows
			T:=map(V^**(n+1),V^**(n+1),1);
			scan(n,j->T=T*(map(V^**j,V^**j,1)**(Rcnum (FF_0,
					if curCotOpts.Equivariant then FF_(j+1) else if curCotOpts.Kth then 1 else 0,BB_i)
				    )**map(V^**(n-1-j),V^**(n-1-j),1)));
			--print i;
			Z=Z*submatrix(T,{(rank W)*ω_i..(rank W)*(ω_i+1)-1},apply(rank W,i->i*(d+1)+d));
			--print Z;
			));
		scan(dims#d,i->scan(n,j-> Z = Z*(Rcden(FF_0,
				if curCotOpts.Equivariant then FF_(j+1) else if curCotOpts.Kth then 1 else 0,BB_i))^(-1)));
    	    	Z
    		)));
	sClass (List,AA) := o -> (L,AA) -> (sClasses(AA,o))_(ind\L);
	sClass (String,AA) :=
	sClass (AryString,AA) := o -> (i,AA) -> (sClasses(AA,o))_(0,ind i);
	segreClass (List,AA) := o -> (L,AA) -> matrix { apply(L,i->segreClass(i,AA,o)) };
	segreClass (List,BB) := o -> (L,BB) -> matrix { apply(L,i->segreClass(i,lastSetup,Partial=>false)) };
	segreClass (String,AA) :=
	segreClass (AryString,AA) := o -> (i,AA) -> (if curCotOpts.Kth then FF_0 else -1)^(inversion i)*sClass(i,AA,o);

	stableClass (List,AA) :=
	stableClass (String,AA) :=
	stableClass (AryString,AA) := o -> (i,AA) -> zeroSection(AA,o) * sClass(i,AA,o);

	chernClass (List,AA) :=
	chernClass (String,AA) :=
	chernClass (AryString,AA) := o -> (i,AA) -> dualZeroSection(AA,o) * segreClass(i,AA,o);
	-- Schubert classes
	schubertClasses AA := o -> (
	    prt := o.Partial === null or o.Partial;
	    (cacheValue (schubertClasses,prt)) (if prt then AA -> fullToPartial schubertClasses(AA,Partial=>false)
	     	else AA -> (
		-- monodromy matrix
		V:=BB^(d+1);
		W:=V^**n;
		Z:=map(BB^1,W,{{rank W-1:0,1}});
		scan(reverse(0..dims#d-1),i->( -- not 0..n-1: slight optimization: don't do trivial rows
			T:=map(V^**(n+1),V^**(n+1),1);
			scan(n,j->T=T*(map(V^**j,V^**j,1)**(Rcz (FF_0,
					if curCotOpts.Equivariant then FF_(j+1) else if curCotOpts.Kth then 1 else 0,BB_i)
				    )**map(V^**(n-1-j),V^**(n-1-j),1)));
			--print i;
			Z=Z*submatrix(T,{(rank W)*ω_i..(rank W)*(ω_i+1)-1},apply(rank W,i->i*(d+1)+d));
			--print Z;
			));
    	    	Z
		)));
	schubertClass (List,AA) := o -> (L,AA) -> (schubertClasses(AA,o))_(ind\L);
	schubertClass (String,AA) :=
	schubertClass (AryString,AA) := o -> (i,AA) -> (schubertClasses(AA,o))_(0,ind i);
	-- duality
	du1 := if curCotOpts.Kth then prepend(FF_0^-1,apply(numgens FF-1,i->FF_(numgens FF-1-i)^-1))
	    else prepend(-FF_0,apply(numgens FF-1,i->-FF_(numgens FF-1-i)));
	du2 := apply(gens BB,x->if curCotOpts.Kth then x^-1 else -x); -- what about AA? TODO
	du := map(BB,BB,du2|du1); -- TODO AA
	sClass' (List,AA) :=
	sClass' (String,AA) :=
	sClass' (AryString,AA) := o -> (i,AA) -> (
	    x := du sClass(if class i === List then reverse\i else reverse i,AA,Partial=>false);
	    if o.Partial === null or o.Partial then fullToPartial x else x
	    );
	-- compared to Mihalcea, missing a (-t)^-D; compared to dual of chernClass, missing q^#
	segreClass' (List,AA) := 
	segreClass' (String,AA) :=
	segreClass' (AryString,AA) := o -> (i,AA) -> (
	    x := du segreClass(if class i === List then reverse\i else reverse i,AA,Partial=>false);
	    if o.Partial === null or o.Partial then fullToPartial x else x
	    );
	-- compared to Mihalcea, missing a (-t)^-D; compared to dual of segreClass, missing q^#
	chernClass' (List,AA) :=
	chernClass' (String,AA) :=
	chernClass' (AryString,AA) := o -> (i,AA) -> dualZeroSection(AA,o) * segreClass'(i,AA,o);
	stableClass' (List,AA) := o -> (L,AA) -> matrix { apply(L,i->stableClass'(i,AA,o)) };
	stableClass' (String,AA) :=
	stableClass' (AryString,AA) := o -> (i,AA) -> (if curCotOpts.Kth then FF_0 else -1)^(2*inversion reverse i+inversion i)*chernClass'(i,AA,o);
	-- power of q is silly TODO better
	-- restriction to fixed points
	if curCotOpts.Equivariant then (
	    restrictMap := i -> map(FF,BB, apply(n,j->FF_((flatten subs i)#j+1)));
	    restrict (AA,AA) :=
	    restrict (BB,AA) := (b,AA) -> vector apply(I,i->(restrictMap i) b); -- where is D?
	    );
	-- pushforwards
	-- find element whose pushforward is nonzero
	local nzpf; -- index of nonzero pushforward basis element
	if curCotOpts.Kth then (
	    nzpf = 0;
	    ) else (
	    -- with normal ordering: product of det line bundles ^ dims of flags product(1..d,i->tautClass(dimdiffs#i,i)^(dims#i))
	    -- with reverse ordering: product(1..d,i->tautClass(dimdiffs#i,i)^(codims#i)) where codim#i = last dims - dims#i
	    nzpf = maxPosition flatten last degrees basis AA; -- we locate it by max degree
	    );
	pushforwardToPoint BB := b -> pushforwardToPoint fullToPartial b;
	pushforwardToPoint AA := a -> (basisCoeffs a)_(nzpf,0);
	pushforwardToPointFromCotangent BB := b -> pushforwardToPoint (zeroSectionInv BB * b);
	pushforwardToPointFromCotangent AA := a -> pushforwardToPoint (zeroSectionInv AA * a);
	--
	(AA,BB,FF,I)
	) else if curCotOpts.Presentation === EquivLoc then (
	if not curCotOpts.Equivariant then error "Equivariant localization requires Equivariant option";
	-- precompute Rcheck-matrices
	V:=FF^(d+1); Rcheck := new IndexedVariableTable; Rcheckz := new IndexedVariableTable;
	scan(n-1,j->Rcheck_j = map(V^**j,V^**j,1)**(Rc (FF_0,FF_(j+1),FF_(j+2)))**map(V^**(n-2-j),V^**(n-2-j),1));
	scan(n-1,j->Rcheckz_j = map(V^**j,V^**j,1)**(Rcz (FF_0,FF_(j+1),FF_(j+2)))**map(V^**(n-2-j),V^**(n-2-j),1));
	-- Module are immutable so can't use them. DiagonalAlgebra aren't
	M := FF^#I;
	D := new DiagonalAlgebra from M;
	fixedPoint := memoize( (segre,i) -> ( -- this returns the restrictions to a given fixed point segre=true: segre; segre=false: schubert
		-- find first descent
		j:=position(0..n-2,k->i#k>i#(k+1));
		tau0:=new AryString from apply(n,k->if k==j then j+1 else if k==j+1 then j else k);
		tau:=map(FF,FF,prepend(FF_0,(drop(gens FF,1))_tau0));
		(tau (fixedPoint(segre,i_tau0)))*(if segre then Rcheck else Rcheckz)_j
		), { (true,ω) => transpose matrix ZZ^((d+1)^n)_(ind ω), (false,ω) => transpose matrix ZZ^((d+1)^n)_(ind ω) } );
	-- segre & schubert classes
	sClass (List,D) := o -> (L,D) -> ( -- should I cacheValue?
		inds := ind \ L;
		map(M,M, apply(I,i->first entries (fixedPoint(true,i))_inds))
		);
	sClass (String,D) :=
	sClass (AryString,D) := o -> (i,D) -> (
	    indi:=ind i;
	    new D from apply(I,ii->(fixedPoint(true,ii))_(0,indi))
	    );
	segreClass (String,D) :=
	segreClass (AryString,D) := o -> (i,D) -> (if curCotOpts.Kth then FF_0 else -1)^(inversion i)*sClass(i,D);
	segreClass (List,D) := o -> (L,D) -> (
	    q := if curCotOpts.Kth then FF_0 else -1;
	    sClass(L,D) * diagonalMatrix apply(L,i->q^(inversion i))
	    );
	chernClass (String,D) :=
	chernClass (AryString,D) := o -> (i,D) -> dualZeroSection D * segreClass(i,D);
	chernClass (List,D) := o -> (L,D) -> matrix dualZeroSection D * segreClass(L,D);
	stableClass (String,D) :=
	stableClass (AryString,D) := o -> (i,D) -> zeroSection D * sClass(i,D);
	stableClass (List,D) := o -> (L,D) -> matrix zeroSection D * sClass(L,D);

	schubertClass (List,D) := o -> (L,D) -> ( -- should I cacheValue?
		inds := ind \ L;
		map(M,M, apply(I,i->first entries (fixedPoint(false,i))_inds))
		);
	schubertClass (String,D) :=
	schubertClass (AryString,D) := o -> (i,D) -> (
	    indi:=ind i;
	    new D from apply(I,ii->(fixedPoint(false,ii))_(0,indi))
	    );

	if curCotOpts.Kth then (
	    weights D := (cacheValue weights) (D -> map(FF^1,M, { apply(I,i->product(n,j->product(n,k->if i#j<i#k then (1-FF_(k+1)/FF_(j+1))^(-1) else 1))) }));
	    zeroSection D := o -> (cacheValue zeroSection) (D -> new D from apply(I,i->product(n,j->product(n,k->if i#j<i#k then 1-FF_0^2*FF_(j+1)/FF_(k+1) else 1))));
	    zeroSectionInv D := (cacheValue zeroSectionInv) (D -> new D from apply(I,i->product(n,j->product(n,k->if i#j<i#k then (1-FF_0^2*FF_(j+1)/FF_(k+1))^(-1) else 1))));
	    dualZeroSection D := (cacheValue dualZeroSection) (D -> new D from apply(I,i->product(n,j->product(n,k->if i#j<i#k then 1-FF_0^-2*FF_(j+1)^-1*FF_(k+1) else 1))));
	    cotweights D := (cacheValue cotweights) (D -> map(FF^1,M, { apply(I,i->product(n,j->product(n,k->if i#j<i#k then (1-FF_(k+1)/FF_(j+1))^(-1)*(1-FF_0^2*FF_(j+1)/FF_(k+1))^(-1) else 1))) }));
	    ) else (
	    weights D := (cacheValue weights) (D -> map(FF^1,M, { apply(I,i->product(n,j->product(n,k->if i#j<i#k then (FF_(j+1)-FF_(k+1))^(-1) else 1))) }));
	    zeroSection D := (cacheValue zeroSection) (D -> new D from apply(I,i->product(n,j->product(n,k->if i#j<i#k then FF_0-FF_(j+1)+FF_(k+1) else 1))));
	    zeroSectionInv D := (cacheValue zeroSectionInv) (D -> new D from apply(I,i->product(n,j->product(n,k->if i#j<i#k then (FF_0-FF_(j+1)+FF_(k+1))^(-1) else 1))));
	    dualZeroSection D := (cacheValue dualZeroSection) (D -> new D from apply(I,i->product(n,j->product(n,k->if i#j<i#k then -FF_0+FF_(j+1)-FF_(k+1) else 1))));
	    cotweights D := (cacheValue cotweights) (D -> map(FF^1,M, { apply(I,i->product(n,j->product(n,k->if i#j<i#k then (FF_(j+1)-FF_(k+1))^(-1)*(FF_0-FF_(j+1)+FF_(k+1))^(-1) else 1))) }));
	    );
	-- Chern classes of tautological bundles
	tautClass (ZZ,ZZ,D) := (j,i,AA) -> new D from apply(I,s->elem(j,apply((subs s)#i,k->FF_(k+1))));
	-- pushforward to point
	pushforwardToPoint D  := m -> ((weights D)*m)_0;
	pushforwardToPointFromCotangent D  := m -> ((cotweights D)*m)_0;
	-- duality
	du = map(FF,FF,if curCotOpts.Kth then prepend(FF_0^-1,apply(n,i->FF_(n-i)^-1)) else prepend(-FF_0,apply(n,i->-FF_(n-i))));
	star := apply(I,i->(j:=reverse i; position(I,i'->i'==j)));
	sClass' (List,D) := (L,D) -> (du sClass(reverse\L,D))^star;
	sClass' (String,D) :=
	sClass' (AryString,D) := (i,D) -> new D from (du sClass(reverse i,D))^star;
	-- compared to Mihalcea, missing a (-t)^-D; compared to dual of chernClass, missing q^#
	segreClass' (List,D) := (L,D) -> (du segreClass(reverse\L,D))^star;
	segreClass' (String,D) :=
	segreClass' (AryString,D) := (i,D) -> new D from (du segreClass(reverse i,D))^star;
	-- compared to Mihalcea, missing a (-t)^-D; compared to dual of segreClass, missing q^#
	chernClass' (String,D) :=
	chernClass' (AryString,D) := (i,D) -> dualZeroSection D * segreClass'(i,D);
	chernClass' (List,D) := (L,D) -> matrix dualZeroSection D * segreClass'(L,D);
	stableClass' (String,D) :=
	stableClass' (AryString,D) := (i,D) -> (if curCotOpts.Kth then FF_0 else -1)^(2*inversion reverse i+inversion i)*chernClass'(i,D);
	stableClass' (List,D) := (L,D) -> (
	    q := if curCotOpts.Kth then FF_0 else -1;
	    chernClass'(L,D) * diagonalMatrix apply(L,i->q^(2*inversion reverse i+inversion i))
	    );
    	--
	lastSetup = D;
	(D,FF,I)
    ) else error "Unknown presentation"
)

end

(M,FF,I)=setupCotangent(1,2,Kth=>true)
segreCls=segreClasses();
segreInv=segreCls^(-1);
Table table(I,I,(i,j)->segreInv*(segreClass i * segreClass j))
Table table(I,I,(i,j)->fugacityVector puzzle(i,j))
oo==ooo

(AA,BB,f,I) = setupCotangent(1,3,Kth=>true,Presentation=>Borel)
segreCls = segreClasses();
P=puzzle("011","101",Generic=>true,Equivariant=>true,Kth=>true)
(segreCls*fugacityVector P)_0 - segreClass(0,1,1)*segreClass(1,0,1)

