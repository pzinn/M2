------------------------------------------
-- Constructors for common Mackey functors
------------------------------------------

BurnsideMackeyFunctor = method()
BurnsideMackeyFunctor(ZZ) := CpMackeyFunctor => (p) -> (
    C := matrix{{1}};
    T := matrix{{0},{1}};
    R := matrix{{1,p}};

    return makeCpMackeyFunctor(p,R,T,C);
)

FixedFreeMackeyFunctor = BurnsideMackeyFunctor

UnderlyingFreeMackeyFunctor = method()
UnderlyingFreeMackeyFunctor(ZZ) := CpMackeyFunctor => (p) -> (
    I := mutableMatrix id_(ZZ^p);                                    --declare identity matrix
    C := matrix(rowPermute(I,0,({p-1}|(toList (0..p-2)))));
    T := matrix({for i to p-1 list 1});
    R := matrix(for i to p-1 list {1});

    return makeCpMackeyFunctor(p,R,T,C);
)

ComplexRepresentationMackeyFunctor = method()
ComplexRepresentationMackeyFunctor(ZZ) := CpMackeyFunctor => (p) -> (
    C := matrix {{1}};
    T := matrix (for i to p-1 list {1});
    R := matrix {for i to p-1 list 1};

    return makeCpMackeyFunctor(p,R,T,C);
)

RealRepresentationMackeyFunctor = method()
RealRepresentationMackeyFunctor(ZZ) := CpMackeyFunctor => (p) -> (
    if p < 3 then (
        return ComplexRepresentationMackeyFunctor p
    )
    else (
        C := matrix {{1}};
        T := matrix (for i to (p-1)//2 list {1});
        R := matrix {{1} | (for i to (p-3)//2 list 2)};

        return makeCpMackeyFunctor(p,R,T,C);
    )
)
