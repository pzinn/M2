export {"setupCotangent", "chernClass",
    "segreClasses","segreClass",
    "schubertClasses","schubertClass",
    "restrict", "fullToPartial", "basisCoeffs",
    "pushforwardToPoint", "pushforwardToPointFromCotangent", "zeroSection",
    "Presentation", "Borel", "EquivLoc"};

cotOpts := opts ++ { Presentation => EquivLoc }
local curCotOpts;

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
    promote(List,R,S) := (L,R1,S1) -> f\L;
    S.baseRings = prepend(R,S.baseRings); -- temporary -- until promotability test improved in enginering.m2
    )
promoteFromMap (Ring,Ring) := (R,S) -> promoteFromMap(R,S,map(S,R))

AryString = new Type of List; -- could we just use sequences?
new AryString from String := (T,s) -> apply(ascii s,i->i-48);
texMath AryString := s -> concatenate between("\\,",apply(s,x -> if class x === String then x else texMath x))
net AryString := toString AryString := s -> concatenate apply(s,toString)
n:=0; d:=0; ω:={}; I:={}; dimdiffs:={}; dims:={};
local subs; protect fixedPoint;--local fixedPoint; -- eww
globalVars = dims0 -> ( -- TODO retire
    dims = if first dims0 == 0 then dims0 else prepend(0,dims0);
    n = last dims;
    subs = s -> apply(#dims,i->positions(s,j->j==i));
    d = #dims - 2; -- # steps - 2 since includes trivial first and last
    dimdiffs = apply(d+1, i-> dims#(i+1)-dims#i);
    -- list of fixed points
    ω=new AryString from splice apply(d+1, i->dimdiffs_i:i);
    I = unique permutations ω; -- unique? eww
    )
q := getSymbol "q"; zbar := getSymbol "zbar";
FK_-1 = frac(factor(ZZ (monoid[q,zbar,DegreeRank=>0]))); -- same as FK_1, really but diff variable name
FK_0 = frac(factor(ZZ (monoid[q,DegreeRank=>0])));
promoteFromMap(FK_0,FK_-1);
FF=AA=BB=null;
local Rc; local Rcnum; local Rcden; local Rcz; -- eww

KTRmatrix = () -> (
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
    Rc = (qq,z1,z2) -> (map(ring z2,FK_-1,{qq,z2/z1}))Rc0;
    Rcnum = (qq,z1,z2) -> (map(ring z2,FK_-1,{qq,z2*z1^(-1)}))Rcnum0;
    Rcden = (qq,z1,z2) -> (map(ring z2,FK_-1,{qq,z2*z1^(-1)}))Rcden0;
    Rcz = (qq,z1,z2) -> (map(ring z2,FK_-1,{qq,z2*z1^(-1)}))Rcz0;
    )
ℏ := getSymbol "ℏ"; xbar := getSymbol "xbar";
FH_-1 = frac(factor(ZZ (monoid[ℏ,xbar]))); -- same as FH_1, really but diff variable name
FH_0 = frac(factor(ZZ (monoid[ℏ])));
promoteFromMap(FH_0,FH_-1);
HTRmatrix = () -> (
    V1:=FH_-1^(d+1); ℏ:=FH_-1_0; xbar:=FH_-1_1;
    Rcnum0:=map(V1^**2,V1^**2,splice flatten table(d+1,d+1,(i,j)->
            if i==j then (i*(d+2),i*(d+2))=>ℏ-xbar
            else ((i*(d+1)+j,j*(d+1)+i)=>xbar,
                (i*(d+1)+j,i*(d+1)+j)=>ℏ)));
    Rcz0:=map(V1^**2,V1^**2,splice flatten table(d+1,d+1,(i,j)->
            if i==j then (i*(d+2),i*(d+2))=>1
            else ((i*(d+1)+j,j*(d+1)+i)=>if i<j then xbar else 0,
                (i*(d+1)+j,i*(d+1)+j)=>1)));
    Rcden0:=ℏ-xbar;
    Rc0 := 1/Rcden0 * Rcnum0;
     -- TODO rewrite next 4 statements better. also do we need hh???
    Rc = (hh,x1,x2) -> (map(ring x2,FH_-1,{hh,x2-x1}))Rc0;
    Rcnum = (hh,x1,x2) -> (map(ring x2,FH_-1,{hh,x2-x1}))Rcnum0;
    Rcden = (hh,x1,x2) -> (map(ring x2,FH_-1,{hh,x2-x1}))Rcden0;
    Rcz = (hh,x1,x2) -> (map(ring x2,FH_-1,{hh,x2-x1}))Rcz0;
    )

debug Core -- to use "generatorSymbols" and "frame"

defineFK = n -> (
    if not FK#?n then (
        z := getSymbol "z"; -- q := getSymbol "q";
        FK_n = factor(frac(ZZ (monoid[q,z_1..z_n,DegreeRank=>0])));
        promoteFromMap(FK_0,FK_n);
        );
    FK_n
    )

defineFH = n -> (
    if not FH#?n then (
        x := getSymbol "x"; -- ℏ := getSymbol "ℏ";
        FH_n = factor(frac(ZZ (monoid[ℏ,x_1..x_n])));
        promoteFromMap(FH_0,FH_n);
        );
    FH_n
    )

-*
local evals;
eval := sym -> ( -- similar to memoize except table of memorized results is outside so can be reset
    sym <- args -> if evals#?(sym,args) then evals#(sym,args)
    else if evals#?sym then evals#(sym,args) = (evals#sym) args
    else error "Set up first";
    sym = value sym;
)
eval zeroSection; -- [G/P] in T*G/P
eval local zeroSectionInv; -- its inverse
eval local weights; -- weights in tangent space at fixed points in G/P
eval local cotweights; -- in T*G/P
eval segreClasses;
eval segreClass;
eval schubertClasses;
eval schubertClass;
*-
-- new paradigm
protect weights;
protect cotweights;
local lastSetup;
mymoize = fun -> (
    fun' := memoize fun;
    x -> (
	if class x === Sequence and #x === 0 then x=lastSetup
	else if (class x === Sequence and not instance(x#0,HashTable)) or (class x =!= Sequence and not instance(x,HashTable)) then x = (lastSetup,x);
	fun' x
    ))
zeroSection1 = method(Dispatch=>Type) -- used internally
zeroSection = mymoize zeroSection1

zeroSectionInv1 = method(Dispatch=>Type)
zeroSectionInv = mymoize zeroSectionInv1

segreClasses1 = method(Dispatch=>Type)
segreClasses = mymoize segreClasses1

segreClass1 = method(Dispatch=>{Type,Thing})
segreClass = mymoize segreClass1

schubertClasses1 = method(Dispatch=>Type)
schubertClasses = mymoize schubertClasses1

schubertClass1 = method(Dispatch=>{Type,Thing})
schubertClass = mymoize schubertClass1

chernClass = new IndexedVariableTable;
clearChernClass := () -> scan(keys chernClass, i -> if class i === Sequence then remove(chernClass,i))

ind := i -> sum(#i,j->(d+1)^j*i_(#i-1-j));

-- set up base ring
setupCommon = () -> (
    if curCotOpts.Kth then (
	FF = if curCotOpts#Equivariant then defineFK n else FK_0;
	KTRmatrix();
	) else (
	FF = if curCotOpts#Equivariant then defineFH n else FH_0;
	HTRmatrix();
        );
--    evals = new MutableHashTable;
)

setupEquivLoc = () -> (
    if not curCotOpts#Equivariant then error "Equivariant localization requires Equivariant option";
    -- precompute R-matrices
    V:=FF^(d+1); Rcheck := new IndexedVariableTable; Rcheckz := new IndexedVariableTable;
    scan(n-1,j->Rcheck_j = map(V^**j,V^**j,1)**(Rc (FF_0,FF_(j+1),FF_(j+2)))**map(V^**(n-2-j),V^**(n-2-j),1));
    scan(n-1,j->Rcheckz_j = map(V^**j,V^**j,1)**(Rcz (FF_0,FF_(j+1),FF_(j+2)))**map(V^**(n-2-j),V^**(n-2-j),1));
    -- the module Hack -- Modules are immutable, so put functions in cache
    lastSetup = M := FF^#I;
    M.cache#fixedPoint = memoize( (segre,i) -> ( -- this returns the restrictions to a given fixed point segre=true: segre; segre=false: schubert
            -- find first descent
            j:=position(0..n-2,k->i#k>i#(k+1));
            tau0:=new AryString from apply(n,k->if k==j then j+1 else if k==j+1 then j else k);
            tau:=map(FF,FF,prepend(FF_0,(drop(gens FF,1))_tau0));
            (tau (M.cache#fixedPoint(segre,i_tau0)))*(if segre then Rcheck else Rcheckz)_j
            ), { (true,ω) => transpose matrix ZZ^((d+1)^n)_(ind ω), (false,ω) => transpose matrix ZZ^((d+1)^n)_(ind ω) } );
--    M.cache#indexes=I; -- TODO use (right now global ind is used)
    if curCotOpts.Kth then (
	M.cache#weights = () -> map(FF^1,M, { apply(I,i->product(n,j->product(n,k->if i#j<i#k then (1-FF_(k+1)/FF_(j+1))^(-1) else 1))) });
	M.cache#zeroSection    = () -> new M from matrix apply(I,i->{product(n,j->product(n,k->if i#j<i#k then 1-FF_0^2*FF_(j+1)/FF_(k+1)        else 1))});
	M.cache#zeroSectionInv = () -> new M from matrix apply(I,i->{product(n,j->product(n,k->if i#j<i#k then (1-FF_0^2*FF_(j+1)/FF_(k+1))^(-1) else 1))});
	M.cache#cotweights = () -> map(FF^1,M, { apply(I,i->product(n,j->product(n,k->if i#j<i#k then (1-FF_(k+1)/FF_(j+1))^(-1)*(1-FF_0^2*FF_(j+1)/FF_(k+1))^(-1) else 1))) });
	) else (
	M.cache#weights = () -> map(FF^1,M, { apply(I,i->product(n,j->product(n,k->if i#j<i#k then (FF_(j+1)-FF_(k+1))^(-1) else 1))) });
	M.cache#zeroSection    = () -> new M from matrix apply(I,i->{product(n,j->product(n,k->if i#j<i#k then FF_0-FF_(j+1)+FF_(k+1)        else 1))});
	M.cache#zeroSectionInv = () -> new M from matrix apply(I,i->{product(n,j->product(n,k->if i#j<i#k then (FF_0-FF_(j+1)+FF_(k+1))^(-1) else 1))});
	M.cache#cotweights = map(FF^1,M, { apply(I,i->product(n,j->product(n,k->if i#j<i#k then (FF_(j+1)-FF_(k+1))^(-1)*(FF_0-FF_(j+1)+FF_(k+1))^(-1) else 1))) });
	);
    -- Chern classes
    clearChernClass();
    scan(d+1, i-> scan(1..dimdiffs#i, j-> chernClass_(j,i)=new M from matrix apply(I,s->{elem(j,apply((subs s)#i,k->FF_(k+1)))})));
    (M,FF,I)
    )

setupBorel = () -> (
    y := getSymbol "y";
    BB0 := FF(monoid[y_1..y_n,DegreeRank=>if curCotOpts.Kth then 0 else 1]); -- in terms of Chern roots
    J := ideal apply(1..n,k->elem(k,gens BB0)
        -if curCotOpts.Equivariant then elem(k,FF_1..FF_n) else if curCotOpts.Kth then binomial(n,k) else 0);
    BB = BB0/J; lastSetup=BB;
    -- Chern classes
    clearChernClass();
    lst := transpose splice apply(d+1, i-> apply(1..dimdiffs#i, j-> 
	    {y_(j,toList(dims#i+1..dims#(i+1))),chernClass_(j,i),elem(j,apply(dims#i..dims#(i+1)-1,k->BB_k))}));
    -- variable name, alias, expression in terms of Chern roots
    R1 := FF monoid new Array from append(lst#0, Degrees=>splice apply(d+1,i->1..dimdiffs#i));
    f := map(BB,R1,lst#2);
    AA = R1 / kernel f;
    scan(lst#1,gens AA,(c,a)-> c <- a);
    promoteFromMap(AA,BB,f*map(R1,AA));
    -- reverse transformation
    fullToPartial FF :=
    fullToPartial BB := b -> (
	if d == n-1 then return (map(AA,BB,gens AA)) b; -- special case of full flag
	AB := FF monoid (BB.generatorSymbols | AA.generatorSymbols); -- no using it
	b = sub(b,AB);
	-- scan(d+1,i->b=expandElem(b,toList(AB_(dims#i)..AB_(dims#(i+1)-1)),toList(AB_(n+dims#i)..AB_(n+dims#(i+1)-1))));
	-- fails because of https://github.com/Macaulay2/M2/issues/2020
	v := seq -> apply(toList seq, j -> AB_j);
	scan(d+1,i->b=expandElem(b,v(dims#i..dims#(i+1)-1),v(n+dims#i..n+dims#(i+1)-1)));
	sub(b,AA)
	);
    zeroSection1 BB := BB -> product(n,j->product(n,k->if ω#j<ω#k then if curCotOpts.Kth then 1-FF_0^2*BB_j*BB_k^(-1) else FF_0-BB_j+BB_k else 1)); -- TODO move if out of def
    zeroSection1 AA := AA -> fullToPartial zeroSection BB;
    zeroSectionInv1 BB := BB -> (zeroSection BB)^(-1);
    zeroSectionInv1 AA := AA -> (zeroSection AA)^(-1);
    -- segre Classes
    segreClasses1 BB := BB -> (
	-- monodromy matrix
	V:=BB^(d+1);
	W:=V^**n;
	Z:=map(BB^1,W,{{rank W-1:0,1}});
	scan(n,i->(
		T:=map(V^**(n+1),V^**(n+1),1);
		scan(n,j->T=T*(map(V^**j,V^**j,1)**(Rcnum (FF_0,
				if curCotOpts#Equivariant then FF_(j+1) else if curCotOpts#Kth then 1 else 0,BB_(n-1-i))
			    )**map(V^**(n-1-j),V^**(n-1-j),1)));
		--print i;
		Z=Z*submatrix(T,{(rank W)*ω_(n-1-i)..(rank W)*(ω_(n-1-i)+1)-1},apply(rank W,i->i*(d+1)+d));
		--print Z;
		));
	scan(n,i->scan(n,j-> Z = Z*(Rcden(FF_0,
			if curCotOpts#Equivariant then FF_(j+1) else if curCotOpts#Kth then 1 else 0,BB_i))^(-1)));
	inds := ind \ I;
	Z_inds
	);
    segreClasses1 AA := AA -> fullToPartial segreClasses BB;
    segreClass1 (AA,VisibleList) :=
    segreClass1 (BB,VisibleList) :=
    segreClass1 (AA,String) :=
    segreClass1 (BB,String) := (X,i) -> (
	i=new AryString from i;
	(segreClasses X)_(0,position(I,j->j==i))
	);
    -- Schubert classes
    schubertClasses1 BB := BB -> (
	-- monodromy matrix
	V:=BB^(d+1);
	W:=V^**n;
	Z:=map(BB^1,W,{{rank W-1:0,1}});
	scan(n,i->(
		T:=map(V^**(n+1),V^**(n+1),1);
		scan(n,j->T=T*(map(V^**j,V^**j,1)**(Rcz (FF_0,
				if curCotOpts#Equivariant then FF_(j+1) else if curCotOpts#Kth then 1 else 0,BB_(n-1-i))
                            )**map(V^**(n-1-j),V^**(n-1-j),1)));
		--print i;
		Z=Z*submatrix(T,{(rank W)*ω_(n-1-i)..(rank W)*(ω_(n-1-i)+1)-1},apply(rank W,i->i*(d+1)+d));
		--print Z;
		));
	inds := ind \ I;
	Z_inds
	);
    schubertClasses1 AA := AA -> fullToPartial schubertClasses BB;
    schubertClass1 (AA,VisibleList) :=
    schubertClass1 (BB,VisibleList) :=
    schubertClass1 (AA,String) :=
    schubertClass1 (BB,String) := (X,i) -> (
	i=new AryString from i;
	(schubertClasses X)_(0,position(I,j->j==i))
	);
    -- restriction to fixed points
    restrictMap := i -> map(FF,BB, apply(n,j->FF_((flatten subs i)#j+1)));
    restrict AA :=
    restrict BB := b -> vector apply(I,i->(restrictMap i) b); -- TODO where is M?
    -- pushforwads
    -- find element whose pushforward is nonzero
    local nzpf; -- index of nonzero pushforward basis element
    local pfsign;
    if curCotOpts.Kth then (
	nzpf = 0;
	pfsign = 1;
	) else (
	-- should be product of det line bundles ^ dims of flags i.e.: product(1..d,i->chernClass_(dimdiffs#i,i)^(dims#i));
	degs := flatten last degrees basis AA;
	nzpf = position(degs, d -> d == max degs);
	pfsign = (-1)^(sum(1..d,i->dims#i*dimdiffs#i));
	);
    pushforwardToPoint BB := b -> pushforwardToPoint fullToPartial b;
    pushforwardToPoint AA := a -> pfsign*(basisCoeffs a)_(nzpf,0);
    pushforwardToPointFromCotangent BB := b -> pushforwardToPoint (zeroSectionInv BB * b);
    pushforwardToPointFromCotangent AA := a -> pushforwardToPoint (zeroSectionInv AA * a);
    --
    (AA,BB,FF,I)
    )

setupCotangent = cotOpts >> o -> dims -> (
    curCotOpts = o;
    if #dims === 0 or unique dims === {0} then error "Please specify nonzero dimensions";
    globalVars dims;
    -- redefine default puzzle opts
    (frame puzzle)#0 = applyPairs((frame puzzle)#0,(k,v) -> (k,if o#?k then o#k else v)); -- TODO use Factor's new "fuse"
    setupCommon();
    if curCotOpts#Presentation === Borel then setupBorel()
    else if curCotOpts#Presentation === EquivLoc then setupEquivLoc()
    else error "Unknown presentation"
    )

-- the methods below are defined for appropriate rings by setup
-- the defs below are just placeholders (try to promote to latest FF) or apply-type
-- restriction to fixed points
restrict = method(Dispatch => Thing)
restrict Thing := r -> try restrict promote(r,BB) else error "Not applicable (set up first?)";
restrict Matrix := m -> matrix apply(flatten entries m,restrict) -- only for one-row matrices

-- from full flag to partial flag
fullToPartial = method(Dispatch => Thing)
fullToPartial Thing := r -> try fullToPartial promote(r,BB) else error "Not applicable (set up first?)";
fullToPartial Matrix := m -> matrix applyTable(entries m,fullToPartial)

-- a simple function that seems like it should already exist
basisCoeffs = x -> lift(last coefficients(x, Monomials => basis ring x),(ring x).basering)

-- Vector methods (for equiv loc)
Vector @ Vector := (v,w) -> new class v from matrix apply(entries v,entries w,(x,y)->{x*y}); -- componentwise multiplication
Vector ^^ ZZ := (v,n) -> new class v from matrix apply(entries v, a -> {a^n}); -- componentwise power

-- segre motivic/SM classes equiv loc
segreClass1 (Vector,VisibleList) :=
segreClass1 (Vector,String) := (M,i) -> (
    i=new AryString from i;
    indi:=ind i; -- TODO rewrite
    new M from matrix apply(I,ii->{(M.cache#fixedPoint(true,ii))_(0,indi)})
    );
segreClasses1 Vector := M -> (
	inds := ind \ I; -- TODO rewrite
--	concatRows apply(I,i->(fixedPoint(true,i))_inds) -- TODO: source/target
	map(M,M, apply(I,i->first entries (M.cache#fixedPoint(true,i))_inds))
	);
-- schubert classes equiv loc
schubertClass1 (Vector,VisibleList) :=
schubertClass1 (Vector,String) := (M,i) -> (
    i=new AryString from i;
    indi:=ind i; -- TODO rewrite
    new M from matrix apply(I,ii->{(M.cache#fixedPoint(false,ii))_(0,indi)}) -- TODO check
    );
schubertClasses1 Vector := M -> (
	inds := ind \ I; -- TODO rewrite
--	concatRows apply(I,i->(fixedPoint(true,i))_inds) -- TODO: source/target
	map(M,M, apply(I,i->first entries (M.cache#fixedPoint(false,i))_inds)) -- TODO check
	);
-- zero section equiv loc
zeroSection1 Vector := M -> M.cache#zeroSection ()

-- pushforward equiv loc or matrix
pushforwardToPoint=method(); -- pushforward to a point from K(G/P)
pushforwardToPoint Thing := r -> try pushforwardToPoint promote(r,AA) else error "Not applicable (set up first?)";
pushforwardToPoint Matrix := m -> if (ring m)#?pushforwardToPoint then matrix applyTable(entries m,pushforwardToPoint) else (target m).cache#weights()*m; -- TODO check second case
pushforwardToPoint Vector := v -> ((class v).cache#weights()*v)_0 -- TODO add check

pushforwardToPointFromCotangent=method(); -- pushforward to a point from K(T^*(G/P))
pushforwardToPointFromCotangent Thing := r -> try pushforwardToPointFromCotangent promote(r,AA) else error "Not applicable (set up first?)";
pushforwardToPointFromCotangent Matrix := m -> if (ring m)#?pushforwardToPointFromCotangent then matrix applyTable(entries m,pushforwardToPointFromCotangent) else (target m).cache#cotweights()*m; -- TODO check
pushforwardToPointFromCotangent Vector := v -> ((class v).cache#cotweights()*v)_0 -- TODO


end

(FF,I)=setupCotangent(1,2,Kth=>true)
segreCls=segreClasses();
segreInv=segreCls^(-1);
Table table(I,I,(i,j)->segreInv*(segreClass i @ segreClass j))

(AA,BB,f,I) = setupCotangent(1,3,Kth=>true,Presentation=>Borel)
segreCls = segreClasses();
P=puzzle("011","101",Generic=>true,Equivariant=>true,Kth=>true)
(segreCls*fugacityVector P)_0 - segreClass(0,1,1)*segreClass(1,0,1)

