--CpMackeyFunctors.m2
newPackage(
    "CpMackeyFunctors",
    Version=>"1.0",
    Date=>"June 24, 2025",
    Authors=>{
        {Name=>"Thomas Brazelton",
            Email=>"brazelton@math.harvard.edu",
            HomePage=>"https://tbrazel.github.io/"},
        {Name=>"Ben Spitz",
            Email=>"benspitz@virginia.edu",
            HomePage=>"https://benspitz.com/"},
        {Name=>"Michael R. Zeng",
            Email=>"zengrf@uw.edu",
            HomePage=>"https://sites.google.com/uw.edu/zengrf/"},
        {Name=>"David Chan",
            Email=>"chandav2@msu.edu",
            HomePage=>"https://sites.google.com/view/davidchanmath"},
        {Name=>"Chenglu Wang",
            Email=>"cwang@math.harvard.edu",
            HomePage=>"https://sites.google.com/uw.edu/zengrf/"},
        {Name=>"Sasha Zotine",
            Email=>"sashahbc@gmail.com",
            HomePage=>"https://sites.google.com/view/szotine/home"},
        {Name=>"Chase Vogeli",
            Email=>"cpv29@cornell.edu",
            HomePage=>"https://chasevoge.li"},
        {Name=>"Add Yourself",
            Email=>"email",
            HomePage=>"url"}
    },
    Headline=>"for working with Mackey functors for cyclic groups of prime order",
    PackageImports=>{},
    PackageExports=>{},
    AuxiliaryFiles => true,
    DebuggingMode=>true
)

load "./CpMackeyFunctors/Code/Helpers.m2"
-- helper commands are internal only, and not exported

-- Code for the CpMackeyFunctor type
load "./CpMackeyFunctors/Code/MackeyFunctor.m2"
export{
    -- CpMackeyFunctors
    "CpMackeyFunctor",
    "makeCpMackeyFunctor",
    "getPrimeOrder",
    "getUnderlyingModule",
    "getFixedModule",
    "getRestriction",
    "getTransfer",
    "getConjugation"
}

load "./CpMackeyFunctors/Code/Constructors.m2"
export{
    "makeBurnsideMackeyFunctor",
    "makeFixedFreeMackeyFunctor",
    "makeUnderlyingFreeMackeyFunctor",
    "makeComplexRepresentationMackeyFunctor",
    "makeRealRepresentationMackeyFunctor",
    "makeZeroMackeyFunctor",
    "makeFixedPointMackeyFunctor",
    "makeOrbitMackeyFunctor"
}

load "./CpMackeyFunctors/Code/Homomorphisms.m2"
export{
    -- Homomorphisms
    "MackeyFunctorHomomorphism",
    "UnderlyingMap",
    "FixedMap"
}

beginDocumentation()

document{
    Key => CpMackeyFunctors,
    Headline => "package for working with Cp-Mackey functors",
    PARA{
        "This package is intended for doing", TEX ///$C_p$///, "-Mackey functor computations."
    },
}

undocumented{
    }

load "./CpMackeyFunctors/Documentation/ConstructorsDoc.m2"


----------------------------
----------------------------
-- Testing
----------------------------
----------------------------

-- load "./tests/HelpersTest.m2"
-- load "./tests/MackeyFunctorsTest.m2"
