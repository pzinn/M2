-- Direct sums
CpMackeyFunctor ++ CpMackeyFunctor := CpMackeyFunctor => (M,N) -> (
    if getPrimeOrder(M) != getPrimeOrder(N) then error("-- Primes not the same (incompatable)");
    T := getTransfer(M) ++ getTransfer(N);
    R := getRestriction(M) ++ getRestriction(N);
    C := getConjugation(M) ++ getConjugation(N);
    p := getPrimeOrder(M);

    makeCpMackeyFunctor(p,R,T,C)
)

-- Kernels
ker MackeyFunctorHomomorphism := CpMackeyFunctor => options -> F -> (
    T := ker F.FixedMap;
    B := ker F.UnderlyingMap;

    C' := inducedMap(B,B,getConjugation(source(F)));
    T' := inducedMap(T,B,getTransfer(source(F)));
    R' := inducedMap(B,T,getRestriction(source(F)));
    p' := getPrimeOrder(source(F));

    makeCpMackeyFunctor(p',R',T',C')
)

-- Cokernels
coker MackeyFunctorHomomorphism := CpMackeyFunctor => F -> (
    T := coker F.FixedMap;
    B := coker F.UnderlyingMap;

    C' := inducedMap(B,B,getConjugation(target(F)));
    T' := inducedMap(T,B,getTransfer(target(F)));
    R' := inducedMap(B,T,getRestriction(target(F)));
    p' := getPrimeOrder(target(F));

    makeCpMackeyFunctor(p',R',T',C')
)
