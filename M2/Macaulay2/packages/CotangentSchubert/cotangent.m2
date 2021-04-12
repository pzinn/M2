export {"setupKT","setupHT","segreClasses","segreClass",
    "setupKTBorel","setupHTBorel","segreClassesBorel","segreClassBorel",
    "restrict"};

-- next two functions should be memoized
elem = (i,vrs) -> sum(subsets(vrs,i), product);
expandElem = (P,vrs,els) -> (
    if P == 0 then return 0;
    c := coefficients(P,Variables=>vrs);
    M := c#0_(0,0); C := c#1_(0,0);
    e := append((first exponents M)_(apply(vrs,index)),0);
    ee := apply(#vrs, i -> e#i - e#(i+1));
    if any(ee, i->i<0) then error "nonsymmetric polynomial";
    Q := P - C * product(#vrs, i -> (elem(i+1,vrs))^(ee#i));
    sub(C,ring first els) * product(#vrs, i -> (els#i)^(ee#i)) + expandElem(Q,vrs,els)
    )

-- build ring of K_T(T*flag)
AryString = new Type of List; -- could we just use sequences?
new AryString from String := (T,s) -> apply(ascii s,i->i-48);
texMath AryString := s -> concatenate between("\\,",apply(s,toString))
net AryString := toString AryString := s -> concatenate apply(s,toString)
n:=0; d:=0; ω:={}; I:={}; dimdiffs:={};
subs := null; fixedPoint := null; -- eww
globalVars = dims -> (
    n = last dims;
    subs = s -> apply(#dims,i->positions(s,j->j==i));
    d = #dims - 2; -- # steps - 2 since includes trivial first and last
    dimdiffs = apply(d+1, i-> dims#(i+1)-dims#i);
    -- list of fixed points
    ω=new AryString from splice apply(d+1, i->dimdiffs_i:i);
    I = unique permutations ω; -- unique? eww
    fixedPoint=null;
    )
BBs=new IndexedVariableTable;
q := getSymbol "q"; zbar := getSymbol "zbar";
FK_-1 = factor(ZZ[q,zbar,DegreeRank=>0]); -- same as FK_1, really but diff variable name (and no frac)
FK_0 = frac(factor(ZZ[q,DegreeRank=>0])); -- init index table
FF=AA=BB=null; segreClassTable=new HashTable;
Rc := Rcnum := Rcden := null; -- eww TEMP

KTRmatrix = () -> (
    V1:=FK_-1^(d+1); q:=FK_-1_0; zbar:=FK_-1_1;
    Rcnum0:=map(V1^**2,V1^**2,splice flatten table(d+1,d+1,(i,j)->
            if i==j then (i*(d+2),i*(d+2))=>1-q^2*zbar
            else ((i*(d+1)+j,j*(d+1)+i)=>q*(1-zbar),
                (i*(d+1)+j,i*(d+1)+j)=>(1-q^2)* if i<j then 1 else zbar)));
    Rcden0:=1-q^2*zbar;
    Rc0 := 1/Rcden0 * promote(Rcnum0,frac FK_-1);
    Rc = (qq,z1,z2) -> (map(ring z2,frac FK_-1,{qq,z2/z1}))Rc0;
    Rcnum = (qq,z1,z2) -> (map(ring z2,FK_-1,{qq,z2*z1^(-1)}))Rcnum0;
    Rcden = (qq,z1,z2) -> (map(ring z2,FK_-1,{qq,z2*z1^(-1)}))Rcden0;
    )

debug Core

AfromB := null; AB=null; Z:=null; -- eww
setupBorel = dims -> (
    y := getSymbol "y";
    if not BBs#?n then (
        BB0 := FF(monoid[y_1..y_n]); -- in terms of Chern roots
        J := ideal apply(1..n,k->sum(subsets(gens BB0,k),product)-sum(subsets(FF_1..FF_n,k),product));
        BBs_n = BB0/J;
        );
    BB = BBs_n;
    c := getSymbol "c"; p := getSymbol "p";
    R1 := FF new Array from apply(d+1, i-> apply(1..dimdiffs#i, j-> c_(i,j))); -- in terms of Chern classes
    f := map(BBs_n,R1,apply(gens R1,v->(
                inds:=(baseName v)#1;
                elem(inds#1,apply(dims#(inds#0)..dims#(inds#0+1)-1,j->BBs_n_j))
                )));
    AA = R1 / kernel f; -- should f be available somehow?
    promote(AA,BB) := (a,XX) -> f lift(a,R1); -- eww
    -- now the reverse transformation
    AfromB = b -> (
        AB := FF monoid (BB0.generatorSymbols | R1.generatorSymbols); -- no using it
        if ring b =!= BB then error "wrong ring";
        b = sub(b,AB);
        -- scan(d+1,i->b=expandElem(b,toList(AB_(dims#i)..AB_(dims#(i+1)-1)),toList(AB_(n+dims#i)..AB_(n+dims#(i+1)-1))));
	-- fails because of https://github.com/Macaulay2/M2/issues/2020
	v := seq -> apply(toList seq, j -> AB_j);
	scan(d+1,i->b=expandElem(b,v(dims#i..dims#(i+1)-1),v(n+dims#i..n+dims#(i+1)-1)));
        sub(b,AA)
        );
    Z=null;
    );

setupKTBorel = dims -> ( -- dims = list of dim(V_i)
    if first dims !=0 then dims=prepend(0,dims);
    setupKT dims;
    setupBorel dims;
    (AA,BB,AfromB,I) -- or whatever
    );
setupKT = dims -> ( -- dims = list of dim(V_i)
    if first dims !=0 then dims=prepend(0,dims);
    globalVars dims;
    z := getSymbol "z";
    if not FK#?n then FK_n = factor(frac(ZZ[q,z_1..z_n,DegreeRank=>0]));
    FF=FK_n;
    KTRmatrix();
    (FF,I)
    );
ℏ := getSymbol "ℏ"; xbar := getSymbol "xbar";
FH_-1 = factor(ZZ[ℏ,xbar]); -- same as FH_1, really but diff variable name (and no frac)
FH_0 = frac(factor(ZZ[ℏ])); -- init index table
HTRmatrix = () -> (
    V1:=FH_-1^(d+1); ℏ:=FH_-1_0; xbar:=FH_-1_1;
    Rcnum0:=map(V1^**2,V1^**2,splice flatten table(d+1,d+1,(i,j)->
            if i==j then (i*(d+2),i*(d+2))=>ℏ-xbar
            else ((i*(d+1)+j,j*(d+1)+i)=>xbar,
                (i*(d+1)+j,i*(d+1)+j)=>ℏ)));
    Rcden0:=ℏ-xbar;
    Rc0 := 1/Rcden0 * promote(Rcnum0,frac FH_-1);
    Rc = (hh,x1,x2) -> (map(ring hh,frac FH_-1,{hh,x2-x1}))Rc0;
    Rcnum = (hh,x1,x2) -> (map(ring hh,FH_-1,{hh,x2-x1}))Rcnum0;
    Rcden = (hh,x1,x2) -> (map(ring hh,FH_-1,{hh,x2-x1}))Rcden0;
    )
setupHTBorel = dims -> ( -- dims = list of dim(V_i)
    if first dims !=0 then dims=prepend(0,dims);
    setupHT dims;
    setupBorel dims;
    (AA,BB,I) -- or whatever
    );
setupHT = dims -> ( -- dims = list of dim(V_i)
    if first dims !=0 then dims=prepend(0,dims);
    globalVars dims;
    x := getSymbol "x";
    if not FH#?n then FH_n = factor(frac(ZZ[ℏ,x_1..x_n]));
    FF=FH_n;
    HTRmatrix();
    (FF,I)
    );

ind := i -> sum(#i,j->(d+1)^j*i_(#i-1-j));

segreClassBorel = i -> (
    if Z === null then segreClassesBorel();
    i=new AryString from i;
    Z_(0,ind i) -- not quite there yet: should be in AA, not BB
    );

-- a different approach to restriction to fixed points
restrictMap := i -> map(FF,BB, apply(n,j->FF_((flatten subs i)#j+1)));
restrict = P -> vector apply(I,i->(restrictMap i) P);

Vector @ Vector := (v,w) -> vector apply(entries v,entries w,times); -- componentwise multiplication

segreClassesBorel = () -> (
    -- check that setup TODO
    -- monodromy matrix
    V:=BB^(d+1);
    W:=V^**n;
    Z=map(BB^1,W,{{rank W-1:0,1}});
    scan(n,i->(
            T:=map(V^**(n+1),V^**(n+1),1);
            scan(n,j->T=T*(map(V^**j,V^**j,1)**(Rcnum (FF_0,FF_(j+1),BB_(n-1-i)))**map(V^**(n-1-j),V^**(n-1-j),1)));
            --            print i;
            Z=Z*submatrix(T,{(rank W)*ω_(n-1-i)..(rank W)*(ω_(n-1-i)+1)-1},apply(rank W,i->i*(d+1)+d));
            --            print Z;
            ));
    scan(n,i->scan(n,j-> Z = Z*(Rcden(FF_0,FF_(j+1),BB_i))^(-1)));
    apply(I, i -> segreClassBorel i)
    -- segreClasssi=segreClasss^(-1);
    );

segreClass = i -> (
    if fixedPoint === null then segreClasses();
    i=new AryString from i;
    indi:=ind i;
    vector apply(I,ii->(fixedPoint ii)_indi)
    );

segreClasses = () -> (
    -- check that setup TODO
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

end

setupKT(1,2)
segreCls=segreClasses();
segreInv=segreCls^(-1);
segreInv*restrict(segreClass{0,1}*segreClass{1,0})
Table table(I,I,(i,j)->segreInv*restrict(segreClass i * segreClass j))

