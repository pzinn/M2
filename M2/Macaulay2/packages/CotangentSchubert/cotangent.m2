export {"setupCotangent",
    "segreClasses","segreClass",
    "restrict", "fullToPartial", "basisCoeffs",
    "pushforwardToPoint", "pushforwardToPointFromCotangent",
    "Presentation", "Borel", "EquivLoc"};

cotOpts := opts ++ { Presentation => EquivLoc }
curCotOpts := null;

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
    )
promoteFromMap (Ring,Ring) := (R,S) -> promoteFromMap(R,S,map(S,R))

AryString = new Type of List; -- could we just use sequences?
new AryString from String := (T,s) -> apply(ascii s,i->i-48);
texMath AryString := s -> concatenate between("\\,",apply(s,x -> if class x === String then x else texMath x))
net AryString := toString AryString := s -> concatenate apply(s,toString)
n:=0; d:=0; ω:={}; I:={}; dimdiffs:={}; dims:={};
subs := null; fixedPoint := null; -- eww
globalVars = dims0 -> (
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
FK_-1 = frac(factor(ZZ[q,zbar,DegreeRank=>0])); -- same as FK_1, really but diff variable name
FK_0 = frac(factor(ZZ[q,DegreeRank=>0]));
promoteFromMap(FK_0,FK_-1);
FF=AA=BB=null;
Rc := Rcnum := Rcden := null; -- eww TEMP

KTRmatrix = () -> (
    V1:=FK_-1^(d+1); q:=FK_-1_0; zbar:=FK_-1_1;
    Rcnum0:=map(V1^**2,V1^**2,splice flatten table(d+1,d+1,(i,j)->
            if i==j then (i*(d+2),i*(d+2))=>1-q^2*zbar
            else ((i*(d+1)+j,j*(d+1)+i)=>q*(1-zbar),
                (i*(d+1)+j,i*(d+1)+j)=>(1-q^2)* if i<j then 1 else zbar)));
    Rcden0:=1-q^2*zbar;
    Rc0 := 1/Rcden0 * Rcnum0;
    Rc = (qq,z1,z2) -> (map(ring z2,FK_-1,{qq,z2/z1}))Rc0;
    Rcnum = (qq,z1,z2) -> (map(ring z2,FK_-1,{qq,z2*z1^(-1)}))Rcnum0;
    Rcden = (qq,z1,z2) -> (map(ring z2,FK_-1,{qq,z2*z1^(-1)}))Rcden0;
    )
Z:=null; -- eww
ℏ := getSymbol "ℏ"; xbar := getSymbol "xbar";
FH_-1 = frac(factor(ZZ[ℏ,xbar])); -- same as FH_1, really but diff variable name
FH_0 = frac(factor(ZZ[ℏ]));
promoteFromMap(FH_0,FH_-1);
HTRmatrix = () -> (
    V1:=FH_-1^(d+1); ℏ:=FH_-1_0; xbar:=FH_-1_1;
    Rcnum0:=map(V1^**2,V1^**2,splice flatten table(d+1,d+1,(i,j)->
            if i==j then (i*(d+2),i*(d+2))=>ℏ-xbar
            else ((i*(d+1)+j,j*(d+1)+i)=>xbar,
                (i*(d+1)+j,i*(d+1)+j)=>ℏ)));
    Rcden0:=ℏ-xbar;
    Rc0 := 1/Rcden0 * Rcnum0;
    Rc = (hh,x1,x2) -> (map(ring x2,FH_-1,{hh,x2-x1}))Rc0;
    Rcnum = (hh,x1,x2) -> (map(ring x2,FH_-1,{hh,x2-x1}))Rcnum0;
    Rcden = (hh,x1,x2) -> (map(ring x2,FH_-1,{hh,x2-x1}))Rcden0;
    )

debug Core -- to use "generatorSymbols" and "frame"

defineFK = n -> FF = (
    if not FK#?n then (
        z := getSymbol "z"; q := getSymbol "q";
        FK_n = factor(frac(ZZ[q,z_1..z_n,DegreeRank=>0]));
        promoteFromMap(FK_0,FK_n);
        );
    FK_n
    )

defineFH = n -> FF = (
    if not FH#?n then (
        x := getSymbol "x"; ℏ := getSymbol "ℏ";
        FH_n = factor(frac(ZZ[ℏ,x_1..x_n]));
        promoteFromMap(FH_0,FH_n);
        );
    FH_n
    )


-- build ring of H/K_T(T*flag)
setupEquivLoc = () -> (
    if not curCotOpts#Equivariant then error "Equivariant localization requires Equivariant option";
    if curCotOpts.Kth then (
        defineFK n;
        KTRmatrix();
        ) else (
        defineFH n;
        HTRmatrix();
        );
    fixedPoint=null;
    (FF,I)
    )

setupBorel = () -> (
    y := getSymbol "y";
    if curCotOpts#Equivariant then setupEquivLoc()
    else if curCotOpts.Kth then (
        FF=FK_0;
        KTRmatrix();
        ) else (
        FF=FH_0;
        HTRmatrix();
        );
    BB0 := FF(monoid[y_1..y_n]); -- in terms of Chern roots
    J := ideal apply(1..n,k->sum(subsets(gens BB0,k),product)
        -if curCotOpts.Equivariant then sum(subsets(FF_1..FF_n,k),product) else if curCotOpts.Kth then binomial(n,k) else 0);
    BB = BB0/J;
    c := getSymbol "c"; p := getSymbol "p";
    R1 := FF new Array from apply(d+1, i-> apply(1..dimdiffs#i, j-> c_(i,j))); -- in terms of Chern classes
    f := map(BB,R1,apply(gens R1,v->(
                inds:=(baseName v)#1;
                elem(inds#1,apply(dims#(inds#0)..dims#(inds#0+1)-1,j->BB_j))
                )));
    AA = R1 / kernel f;
    ff := f*map(R1,AA); -- should ff be available somehow?
    promoteFromMap(AA,BB,ff);
    Z=null;
    (AA,BB,FF,I)
    )

-- now the reverse transformation
fullToPartial = method(Dispatch => Thing)
fullToPartial RingElement := b -> (
    if ring b =!= BB then try b = promote(b,BB) else error "wrong ring";
    AB := FF monoid (BB.generatorSymbols | AA.generatorSymbols); -- no using it
    b = sub(b,AB);
    -- scan(d+1,i->b=expandElem(b,toList(AB_(dims#i)..AB_(dims#(i+1)-1)),toList(AB_(n+dims#i)..AB_(n+dims#(i+1)-1))));
    -- fails because of https://github.com/Macaulay2/M2/issues/2020
    v := seq -> apply(toList seq, j -> AB_j);
    scan(d+1,i->b=expandElem(b,v(dims#i..dims#(i+1)-1),v(n+dims#i..n+dims#(i+1)-1)));
    sub(b,AA)
    );
fullToPartial Matrix := m -> matrix applyTable(entries m,fullToPartial)

-- a simple function that seems like it should already exist
basisCoeffs = x -> lift(last coefficients(x, Monomials => basis ring x),(ring x).basering)

setupCotangent = cotOpts >> o -> dims -> (
    curCotOpts = o;
    if #dims === 0 or unique dims === {0} then error "Please specify nonzero dimensions";
    globalVars dims;
    -- redefine default puzzle opts
    (frame puzzle)#0 = applyPairs((frame puzzle)#0,(k,v) -> (k,if o#?k then o#k else v)); -- TODO use Factor's new "fuse"
    if curCotOpts#Presentation === Borel then setupBorel()
    else if curCotOpts#Presentation === EquivLoc then setupEquivLoc()
    else error "Unknown presentation"
    )

ind := i -> sum(#i,j->(d+1)^j*i_(#i-1-j));

segreClassBorel = i -> (
    if Z === null then segreClassesBorel();
    i=new AryString from i;
    Z_(0,ind i) -- in BB. should it be in AA?
    );

-- restriction to fixed points in the Borel presentation.
restrictMap := i -> map(FF,BB, apply(n,j->FF_((flatten subs i)#j+1)));
restrict = method(Dispatch => Thing)
restrict RingElement := b -> (
    if curCotOpts === null then error "Set up first";
    if not curCotOpts#Equivariant then error "needs equivariance";
    if ring b =!= BB then try b = promote(b,BB) else error "wrong ring";
    vector apply(I,i->(restrictMap i) b)
    );
restrict Matrix := m -> matrix apply(flatten entries m,restrict) -- only for one-row matrices

Vector @ Vector := (v,w) -> vector apply(entries v,entries w,times); -- componentwise multiplication

segreClassesBorel = () -> (
    -- monodromy matrix
    V:=BB^(d+1);
    W:=V^**n;
    Z=map(BB^1,W,{{rank W-1:0,1}});
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
    matrix {apply(I, i -> segreClassBorel i)}
    );

segreClassEquivLoc = i -> (
    if fixedPoint === null then segreClassesEquivLoc();
    i=new AryString from i;
    indi:=ind i;
    vector apply(I,ii->(fixedPoint ii)_indi)
    );

segreClassesEquivLoc = () -> (
    V:=FF^(d+1); Rcheck := new IndexedVariableTable;
    scan(n-1,j->Rcheck_j = map(V^**j,V^**j,1)**(Rc (FF_0,FF_(j+1),FF_(j+2)))**map(V^**(n-2-j),V^**(n-2-j),1)); -- precompute R-matrices
    indω:=ind ω;
    vecω:=vector apply((d+1)^n,j->if j==indω then 1 else 0);
    fixedPoint = memoize( i -> ( -- this returns the restrictions to a given fixed point
            if i === ω then return vecω;
            -- find first descent
            j:=position(0..n-2,k->i#k>i#(k+1));
            tau0:=new AryString from apply(n,k->if k==j then j+1 else if k==j+1 then j else k);
            tau:=map(FF,FF,prepend(FF_0,(drop(gens FF,1))_tau0));
            Rcheck_j*(tau (fixedPoint i_tau0))
            ));
    inds := ind \ I;
    matrix apply(I,i->(entries fixedPoint i)_inds)
    );

segreClass = i -> (
    if curCotOpts === null then error "Set up first";
    if curCotOpts#Presentation === Borel then segreClassBorel i
    else if curCotOpts#Presentation === EquivLoc then segreClassEquivLoc i
    else error "Unknown presentation"
    )

segreClasses = i -> (
    if curCotOpts === null then error "Set up first";
    if curCotOpts#Presentation === Borel then segreClassesBorel i
    else if curCotOpts#Presentation === EquivLoc then segreClassesEquivLoc i
    else error "Unknown presentation"
    )


pushforwardToPoint=method(); -- pushforward to a point from K(G/B)
pushforwardToPoint RingElement := pushforwardToPoint Number := x -> (
    if curCotOpts === null then error "Set up first";
    if curCotOpts#Presentation =!= Borel then error "Borel presentation only";
    (basisCoeffs x)_(0,0)
    )
pushforwardToPoint Matrix := m -> (
    if curCotOpts === null then error "Set up first";
    if curCotOpts#Presentation === Borel then
    matrix applyTable(entries m,pushforwardToPoint)
    else null
    -- TODO: multiplication by appropriate thingie
    )
pushforwardToPoint Vector := v -> vector pushforwardToPoint matrix v

local zeroSection;

pushforwardToPointFromCotangent=method(); -- pushforward to a point from K(T^*(G/B))
pushforwardToPointFromCotangent RingElement := pushforwardToPointFromCotangent Number := x -> (
    if curCotOpts === null then error "Set up first";
    if curCotOpts#Presentation =!= Borel then error "Borel presentation only";
    (basisCoeffs x*zeroSection)_(0,0) -- TODO: define this
    )
pushforwardToPointFromCotangent Matrix := m -> (
    if curCotOpts === null then error "Set up first";
    if curCotOpts#Presentation === Borel then
    matrix applyTable(entries m,pushforwardToPointFromCotangent)
    else null
    -- TODO: multiplication by appropriate thingie
    )
pushforwardToPointFromCotangent Vector := v -> vector pushforwardToPointFromCotangent matrix v

end

(FF,I)=setupCotangent(1,2,Kth=>true)
segreCls=segreClasses();
segreInv=segreCls^(-1);
Table table(I,I,(i,j)->segreInv*(segreClass i @ segreClass j))

(AA,BB,f,I) = setupCotangent(1,3,Kth=>true,Presentation=>Borel)
segreCls = segreClasses();
P=puzzle("011","101",Generic=>true,Equivariant=>true,Kth=>true)
(segreCls*fugacityVector P)_0 - segreClass(0,1,1)*segreClass(1,0,1)

