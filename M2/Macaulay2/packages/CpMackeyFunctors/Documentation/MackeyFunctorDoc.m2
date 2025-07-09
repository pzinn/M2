doc ///
    Key
        CpMackeyFunctor
    Headline
        The type of Cp Mackey functors
    Description
        Text
            This documents the @TO2(Type,"type")@ of $C_p$-Mackey functors. A $C_p$-Mackey functor is encoded as a @TO2(HashTable, "hash table")@ with keys through which we can pull various parts of the data.
        Text
            The keys available (internally) in the hash table are as follows, together with their external {\tt get} methods:

            @UL {
                (TT ".PrimeOrder", " yields the ", TO2(ZZ,"order"), " of the cyclic group over which we're working. Accesible via the  ", TO "getPrimeOrder", " method"),
                (TT ".Underlying",
                " encodes the ", TO "module", " ",
                TEX"$M(C_p/e)$",
                ". It can be accessed via the ",
                TO "getUnderlyingModule", " method."),
                (TT ".Fixed",
                " encodes the ",TO "module", " ",
                TEX"$M(C_p/C_p)$",
                ". It can be accessed via the ",
                TO "getFixedModule", " method."),
                (TT ".Res",
                " encodes the restriction ", TO2("Macaulay2Doc :: module homomorphisms","homomorphism")," ",
                TEX"$M(C_p/C_p) \\to M(C_p/e)$",
                ". It can be accessed via the ",
                TO "getRestriction", " method."),
                (TT ".Tr",
                " encodes the transfer ", TO2("Macaulay2Doc :: module homomorphisms","homomorphism")," ",
                TEX"$M(C_p/e) \\to M(C_p/C_p)$",
                ". It can be accessed via the ",
                TO "getTransfer", " method."),
                (TT ".Conj",
                " encodes the conjugation ", TO2("Macaulay2Doc :: module homomorphisms","homomorphism")," ",
                TEX"$M(C_p/e) \\to M(C_p/e)$",
                ". It can be accessed via the ",
                TO "getConjugation", " method."),
            }@

            {\bf Constructing new Mackey functors:}

            {\bf Operations with Mackey functor types:} One of the first operations to know about is @TO2((prune,CpMackeyFunctor),"pruning")@ a $C_p$-Mackey functor. There is more information at the prune command, but this basically takes the data of a Mackey functor and attempts to simplify it and make it more reader-friendly. Pruning is the only unary operation on $C_p$-Mackey functors that we consider. In terms of binary (and $n$-ary) operations, there are a few, including:

            @UL {
                ("the ", TO2(boxProduct,"box product"), " of two Mackey functors, via  ", TT "M**N"),
                ("the ", TO2(directSum,"direct sum")," of two Mackey functors, as ", TT"M++N"),
                ("the ", TO2(InternalHom,"internal hom"), " of two Mackey functors")
            }@
    SeeAlso
        "background on Mackey functors"
        CpMackeyFunctor
///

doc ///
    Key
        getPrimeOrder
    Headline
        getPrimeOrder
    Usage
        getPrimeOrder M
    Inputs
        M : CpMackeyFunctor
    Outputs
        p : ZZ
            the order of the cyclic group $C_p$ over which $M$ is defined
    Description
        Text
            Given a Mackey functor $M$ defined over $C_p$, this method returns $p$.

    SeeAlso
        "background on Mackey functors"
        CpMackeyFunctor
///

doc ///
    Key
        getUnderlyingModule
    Headline
        getUnderlyingModule
    Usage
        getUnderlyingModule M
    Inputs
        M : CpMackeyFunctor
    Outputs
        : Module
            the underlying module $M(C_p/e)$
    Description
        Text
            Returns the underlying module $M(C_p/e)$ of any Mackey functor $M$.
    SeeAlso
        "background on Mackey functors"
        CpMackeyFunctor
        getFixedModule
///

doc ///
    Key
        getFixedModule
    Headline
        getFixedModule
    Usage
        getFixedModule M
    Inputs
        M : CpMackeyFunctor
    Outputs
        : Module
            the fixed module $M(C_p/C_p)$
    Description
        Text
            Returns the underlying fixed module $M(C_p/C_p)$ of any Mackey functor $M$.
    SeeAlso
        "background on Mackey functors"
        CpMackeyFunctor
        getUnderlyingModule
///

doc ///
    Key
        getRestriction
    Headline
        getRestriction
    Usage
        getRestriction M
    Inputs
        M : CpMackeyFunctor
    Outputs
        : Matrix
            the restriction map in $M$
    Description
        Text
            Returns the restriction homomorphism in $M$.
    SeeAlso
        "background on Mackey functors"
        CpMackeyFunctor
        getTransfer
        getConjugation
///

doc ///
    Key
        getTransfer
    Headline
        getTransfer
    Usage
        getTransfer M
    Inputs
        M : CpMackeyFunctor
    Outputs
        : Matrix
            the transfer map in $M$
    Description
        Text
            Returns the transfer homomorphism in $M$.
    SeeAlso
        "background on Mackey functors"
        CpMackeyFunctor
        getRestriction
        getConjugation
///

doc ///
    Key
        getConjugation
    Headline
        getConjugation
    Usage
        getConjugation M
    Inputs
        M : CpMackeyFunctor
    Outputs
        : Matrix
            the conjugation map in $M$
    Description
        Text
            Returns the conjugation homomorphism in $M$.
    SeeAlso
        "background on Mackey functors"
        CpMackeyFunctor
        getRestriction
        getTransfer
///

doc ///
    Key
        makeCpMackeyFunctor
        (makeCpMackeyFunctor,ZZ,Matrix,Matrix,Matrix)
    Headline
        make a Cp Mackey functor
    Usage
        makeCpMackeyFunctor(p,R,T,C)
    Inputs
        p : ZZ
            a prime number
        R : Matrix
            a restriction homomorphim
        T : Matrix
            a transfer homomorphim
        C : Matrix
            a conjugation homomorphim
    Outputs
        : CpMackeyFunctor
    Description
        Text
            Builds an instance of the @TO("CpMackeyFunctor")@ type from the data of a prime number and three matrices, encoding restriction, transfer, and conjugation, respectively. The method will verify whether the input data yields a valid Mackey functor.
    SeeAlso
        "background on Mackey functors"
        CpMackeyFunctor
///

doc ///
    Key
        (prune,CpMackeyFunctor)
    Headline
        prune a CpMackeyFunctor
    SeeAlso
        "background on Mackey functors"
        CpMackeyFunctor
///