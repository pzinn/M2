doc ///
Node
    Key
        resolution
        (resolution,CpMackeyFunctor, ZZ)
        [resolution,DegreeLimit]
        [resolution,FastNonminimal]
        [resolution,HardDegreeLimit]
        [resolution,LengthLimit]
        [resolution,PairLimit]
        [resolution,ParallelizeByDegree]
        [resolution,SortStrategy]
        [resolution,StopBeforeComputation]
        [resolution,Strategy]
        [resolution,SyzygyLimit]
    Headline
        constructs a free Mackey functor resolution of specified length
    Usage
        resolution(M,n)
    Inputs
        M : CpMackeyFunctor
            a Mackey functor to be resolved
        n : ZZ
            length of desired free resolution
    Outputs
        : List
            a list containing the morphisms in the free resolution
    Description
        Text
            This method returns a free resolution of the Mackey functor $M$ up to the $n$th term.  The resolution is implemented as a list of Mackey functor homomorphisms.
        Example
            M = makeZeroOnUnderlyingMackeyFunctor(3,ZZ^1);
            resolution(M,3)
    Caveat
        Text
            The optional inputs listed on this page are coming from overloading the @TO("OldChainComplexes :: resolution")@ method, and don't have any effect on computing resolutions for Mackey functors.

Node
    Key
        resolutionCohomological
        (resolutionCohomological, CpMackeyFunctor, ZZ)
    Headline
        constructs a free resolution in cohomological Mackey functors of specified length
    Usage
        resolutionCohomological(M,n)
    Inputs
        M : CpMackeyFunctor
            a cohomological Mackey functor to be resolved
        n : ZZ
            length of desired free resolution
    Outputs
        : List
            a list containing the morphisms in the free resolution
    Description
        Text
            This method returns a free resolution of a cohomological Mackey functor by free cohomological Mackey functors.  The resolution is implemented as a list of Mackey functor homomorphisms.
        Example
            N = cokernel(matrix({{3}}));
            M = makeZeroOnUnderlyingMackeyFunctor (3,N);
            resolutionCohomological(M,4)
///
