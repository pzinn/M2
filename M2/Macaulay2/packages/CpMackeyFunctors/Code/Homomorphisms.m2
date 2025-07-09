protect symbol Domain
protect symbol Codomain

MackeyFunctorHomomorphism = new Type of HashTable
MackeyFunctorHomomorphism.synonym = "Mackey Functor homomorphism"

-- For magic
isMorphism MackeyFunctorHomomorphism := f -> true
-- Hack to make matrix() work
ring MackeyFunctorHomomorphism := f -> ZZ
promote (MackeyFunctorHomomorphism, ZZ) := (f,R) -> f

isWellDefined MackeyFunctorHomomorphism := Boolean => F ->(
    -- Verify the keys are correct
    if not (F#?Domain and F#?Codomain and F#?UnderlyingMap and F#?FixedMap) then return false;

    -- Verify domain and codomain are indeed Mackey functors
    if not (class F.Domain === CpMackeyFunctor and class F.Codomain === CpMackeyFunctor) then return false;

    -- Verify that the primes are the same
    if not (F.Domain.PrimeOrder == F.Codomain.PrimeOrder) then return false;

    -- Verify F.UnderlyingMap and F.FixedMap have the right domain and codomain
    if not (source F.UnderlyingMap == getUnderlyingModule F.Domain and target F.UnderlyingMap == getUnderlyingModule F.Codomain) then return false;
    if not (source F.FixedMap == getFixedModule F.Domain and target F.FixedMap == getFixedModule F.Codomain) then return false;

    -- Check commutes with restriction
    if not (F.UnderlyingMap * F.Domain.Res == F.Codomain.Res * F.FixedMap) then (print " -- the given morphism does not commute with restriction"; return false);

    -- Check commutes with transfer
    if not (F.FixedMap * F.Domain.Tr == F.Codomain.Tr * F.UnderlyingMap) then (print " -- the given morphism does not commute with transfer"; return false);

    -- Check commutes with conjugation
    if not (F.UnderlyingMap * F.Domain.Conj == F.Codomain.Conj * F.UnderlyingMap) then (print " -- the given morphism does not commute with conjugation"; return false);

    true
)

-- Arguments:
-- 1. Target
-- 2. Source
-- 3. Underlying map
-- 4. Fixed-point map
map(CpMackeyFunctor, CpMackeyFunctor, Matrix, Matrix) := MackeyFunctorHomomorphism => opts -> (N,M,u,f) -> (
    F := new MackeyFunctorHomomorphism from {
        symbol Codomain => N,
        symbol Domain => M,
        symbol UnderlyingMap => map(getUnderlyingModule(N),getUnderlyingModule(M),u),
        symbol FixedMap =>  map(getFixedModule(N),getFixedModule(M),f),
        symbol cache => new CacheTable
        };
    if isWellDefined F then (
        return F
    )
    else (
        error "Mackey Functor homomorphism is not well-defined";
	)
)

source(MackeyFunctorHomomorphism) := CpMackeyFunctor => F -> (
    return F.Domain
)

target(MackeyFunctorHomomorphism) := CpMackeyFunctor => F -> (
    return F.Codomain
)

CpMackeyFunctor#id = X -> map(X, X, id_(X.Underlying), id_(X.Fixed))

-- This is the linearization map A -> RU
complexLinearizationMap = method()
complexLinearizationMap(ZZ) := MackeyFunctorHomomorphism => p -> (
    map(makeComplexRepresentationMackeyFunctor p, makeBurnsideMackeyFunctor p, matrix {{1}}, matrix {{1,1}} || matrix (for i to p-2 list {0,1}))
)

-- This is the linearization map A -> RO
realLinearizationMap = method()
realLinearizationMap(ZZ) := MackeyFunctorHomomorphism => p -> (
    RO := makeRealRepresentationMackeyFunctor p;
    map(RO, makeBurnsideMackeyFunctor p, matrix {{1}}, matrix {{1,1}} || matrix (for i to (rank (getFixedModule RO) - 2) list {0,1}))
)

-- Given a Mackey functor M and vector x in fixed module, produce map A -> M
makeUniversalMapFixed = method()
makeUniversalMapFixed(CpMackeyFunctor,Vector) := MackeyFunctorHomomorphism => (M,x) -> (
    X := matrix x;
    if not isSubset(image X, M.Fixed) then (
        error "element is not in fixed module";
    )
    else (
        p := M.PrimeOrder;
        A := makeFixedFreeMackeyFunctor(p);
        U := M.Res * X;
        F := X | (M.Tr * M.Res * X);
        return map(M, A, U, F);
    )
)

-- Given a Mackey functor M and n columns in fixed, produce map A^n -> M
makeUniversalMapFixed(CpMackeyFunctor,Matrix) := MackeyFunctorHomomorphism => (M,X) -> (
    n := numColumns X;
    return blockMatrixMackeyFunctorHomomorphism{for i to n-1 list makeUniversalMapFixed(M,X_i)};
)

-- Given a Mackey functor M and vector x in underlying module, produce map B -> M
makeUniversalMapUnderlying = method()
makeUniversalMapUnderlying(CpMackeyFunctor,Vector) := MackeyFunctorHomomorphism => (M,x) -> (
    X := matrix x;

    if not isSubset(image X, M.Underlying) then (
        error "element is not in underlying module";
    )
    else (
        p := M.PrimeOrder;
        B := makeUnderlyingFreeMackeyFunctor(p);
        U := matrix {for i to p-1 list ((M.Conj)^i) * X};
        F := M.Tr * X;
        return map(M, B, U, F);
    )
)

-- Given a Mackey functor M and n columns in underlying, produce map B^n -> M
makeUniversalMapUnderlying(CpMackeyFunctor,Matrix) := MackeyFunctorHomomorphism => (M,X) -> (
    n := numColumns X;
    return blockMatrixMackeyFunctorHomomorphism{for i to n-1 list makeUniversalMapUnderlying(M,X_i)};
)

-- Given:
-- a Mackey functor M,
-- a matrix of n elements X in fixed, and
-- a matrix of m elements Y in underlying,
-- return the universal map A^n ++ B^m -> M
makeUniversalMap = method()
makeUniversalMap(CpMackeyFunctor,Matrix,Matrix) := MackeyFunctorHomomorphism => (M,X,Y) -> (
    return makeUniversalMapFixed(M,X) | makeUniversalMapUnderlying(M,Y)
)

-- Arithmetic operations
-- ZZ-linear operations
MackeyFunctorHomomorphism + MackeyFunctorHomomorphism := MackeyFunctorHomomorphism => (f,g) -> (
    if source f != source g then error("-- sources of maps must agree");
    if target f != target g then error("-- targets of maps must agree");
    map(target f, source g, f.UnderlyingMap + g.UnderlyingMap, f.FixedMap + g.FixedMap)
    )

ZZ * MackeyFunctorHomomorphism := MackeyFunctorHomomorphism => (n, f) -> (
    map(target f, source f, n * f.UnderlyingMap, n * f.FixedMap)
    )

-- Function composition
MackeyFunctorHomomorphism * MackeyFunctorHomomorphism := MackeyFunctorHomomorphism => (G,F) ->(
    -- todo: uncomment after Sasha pushes == method
    -- if not (F.Codomain == G.Domain) then error "Mackey functor maps are not composable";
    map(G.Codomain, F.Domain, G.UnderlyingMap * F.UnderlyingMap, G.FixedMap * F.FixedMap)
)

-- Direct sums of homomorphisms
MackeyFunctorHomomorphism.directSum = args -> (
    if not same ((args/source)/getPrimeOrder) then error "-- Prime not compatible";
    Src := directSum(args/source);
    Tgt := directSum(args/target);
    B := directSum(apply(args,a->a.UnderlyingMap));
    T := directSum(apply(args,a->a.FixedMap));

    map(Tgt,Src,B,T)
    )
MackeyFunctorHomomorphism ++ MackeyFunctorHomomorphism := MackeyFunctorHomomorphism => (F, G) -> MackeyFunctorHomomorphism.directSum(F, G)
directSum MackeyFunctorHomomorphism := MackeyFunctorHomomorphism => F -> MackeyFunctorHomomorphism.directSum(1 : F)

getUnderlyingMap = method()
getUnderlyingMap(MackeyFunctorHomomorphism) := CpMackeyFunctor => F -> (
    F.UnderlyingMap
)

getFixedMap = method()
getFixedMap(MackeyFunctorHomomorphism) := CpMackeyFunctor => F -> (
    F.FixedMap
)

isIsomorphism(MackeyFunctorHomomorphism) := Boolean => F -> (
    isTrivialMackeyFunctor ker F and isTrivialMackeyFunctor coker F
)

isTrivialMackeyFunctor = method()
isTrivialMackeyFunctor(CpMackeyFunctor) := Boolean => F -> (
    getFixedModule(F) == 0 and getUnderlyingModule(F) == 0
)

-- Equality of morphisms
MackeyFunctorHomomorphism == MackeyFunctorHomomorphism := Boolean => (f,g) -> (
    if source f != source g then return false;
    if target f != target g then return false;
    if f.UnderlyingMap != g.UnderlyingMap then return false;
    if f.FixedMap != g.FixedMap then return false;
    true
    )


-- block homomorphisms
MackeyFunctorHomomorphism | MackeyFunctorHomomorphism := MackeyFunctorHomomorphism => MackeyFunctorHomomorphism.concatCols = maps -> (
    if not all(maps, f -> target f === target maps#0) or not all(maps, f -> getPrimeOrder source f === getPrimeOrder source maps#0) then
        error "MackeyFunctorHomomorphism.concatCols: all maps must have the same target and prime order";
    if #maps === 0 then
        error "MackeyFunctorHomomorphism.concatCols: no maps provided";
    map(target maps#0, directSum apply(maps, source), concatCols apply(maps, getUnderlyingMap), concatCols apply(maps, getFixedMap))
)

MackeyFunctorHomomorphism || MackeyFunctorHomomorphism := MackeyFunctorHomomorphism => MackeyFunctorHomomorphism.concatRows = maps -> (
    if not all(maps, f -> source f === source maps#0) or not all(maps, f -> getPrimeOrder source f === getPrimeOrder source maps#0) then
        error "MackeyFunctorHomomorphism.concatRows: all maps must have the same source and prime order";
    if #maps === 0 then
        error "MackeyFunctorHomomorphism.concatRows: no maps provided";
    map(directSum apply(maps, target), source maps#0, concatRows apply(maps, getUnderlyingMap), concatRows apply(maps, getFixedMap))
)

MackeyFunctorHomomorphism.concatBlocks = maps -> MackeyFunctorHomomorphism.concatRows apply(maps, MackeyFunctorHomomorphism.concatCols)
MackeyFunctorHomomorphism.matrix = opts -> MackeyFunctorHomomorphism.concatBlocks

blockMatrixMackeyFunctorHomomorphism = MackeyFunctorHomomorphism.concatBlocks
