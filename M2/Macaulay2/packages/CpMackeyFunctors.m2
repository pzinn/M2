--CpMackeyFunctors.m2
newPackage(
    "CpMackeyFunctors",
    Version=>"1.0",
    Date=>"June 24, 2025",
    Authors=>{
        {Name=>"Thomas Brazelton",
            Email=>"brazelton@math.harvard.edu",
            HomePage=>"https://tbrazel.github.io/"},
        {Name=>"David Chan",
            Email=>"chandav2@msu.edu",
            HomePage=>"https://sites.google.com/view/davidchanmath"},
        {Name=>"Benjamin Mudrak",
            Email=>"bmudrak@purdue.edu",
            HomePage=>"https://www.math.purdue.edu/people/grad/index.html?student=bmudrak"},
        {Name=>"Ben Spitz",
            Email=>"benspitz@virginia.edu",
            HomePage=>"https://benspitz.com/"},
        {Name=>"Chase Vogeli",
            Email=>"cpv29@cornell.edu",
            HomePage=>"https://chasevoge.li"},
        {Name=>"Chenglu Wang",
            Email=>"cwang@math.harvard.edu",
            HomePage=>"https://sites.google.com/uw.edu/zengrf/"},
        {Name=>"Michael R. Zeng",
            Email=>"zengrf@uw.edu",
            HomePage=>"https://sites.google.com/uw.edu/zengrf/"},
        {Name=>"Sasha Zotine",
            Email=>"sashahbc@gmail.com",
            HomePage=>"https://sites.google.com/view/szotine/home"}
    },
    HomePage=>"",   -- this is needed to avoid the following error:
                    -- Warning: The "CpMackeyFunctors" package provides insufficient citation data: howpublished.
    Headline=>"for working with Mackey functors for cyclic groups of prime order",
    PackageImports=>{},
    -- One day: export Complexes and use that framework for computing derived functors
    PackageExports=>{},
    AuxiliaryFiles => true,
    DebuggingMode=>true
)

importFrom_Core {
    "concatBlocks", "concatCols", "concatRows",
    "isMorphism", "isAbelianCategory",
    }

load "./CpMackeyFunctors/Code/Helpers.m2"
-- helper commands are internal only, and not exported

-- Code for the CpMackeyFunctor type
load "./CpMackeyFunctors/Code/MackeyFunctor.m2"
export{
    "CpMackeyFunctor",
    "makeCpMackeyFunctor",
    "getPrimeOrder",
    "getUnderlyingModule",
    "getFixedModule",
    "getRestriction",
    "getTransfer",
    "getConjugation"
}

-- Constructor files for making various Mackey functors
load "./CpMackeyFunctors/Code/Constructors.m2"
export{
    "makeBurnsideMackeyFunctor",
    "makeFixedFreeMackeyFunctor",
    "makeUnderlyingFreeMackeyFunctor",
    "makeComplexRepresentationMackeyFunctor",
    "makeRealRepresentationMackeyFunctor",
    "makeZeroMackeyFunctor",
    "makeFixedPointMackeyFunctor",
    "makeOrbitMackeyFunctor",
    "makeFixedTrivMackeyFunctor",
    "makeFixedSignMackeyFunctor"
}

-- Methods for working with homomorphisms between Mackey functors
load "./CpMackeyFunctors/Code/Homomorphisms.m2"
export{
    -- Homomorphisms
    "MackeyFunctorHomomorphism",
    "UnderlyingMap",
    "FixedMap",
    "complexLinearizationMap",
    "realLinearizationMap",
    "makeUniversalMapFixed",
    "makeUniversalMapUnderlying",
    "makeUniversalMap",
    "getUnderlyingMap",
    "getFixedMap",
    "isTrivialMackeyFunctor",
    "concatBlocks",
    "blockMatrixMackeyFunctorHomomorphism"
}

-- Methods for working in the abelian category Mack_p.
-- Note that ++ and ker and coker are already methods, hence exported
load "./CpMackeyFunctors/Code/AbelianCategory.m2"

-- Methods for forming the abelian group of homomorphisms between two Mackey functors
-- Similarly Hom is already an exported method
load "./CpMackeyFunctors/Code/HomGroup.m2"

load "./CpMackeyFunctors/Code/BoxProduct.m2"
export{
    "boxProduct"
}

load "./CpMackeyFunctors/Code/Resolutions.m2"
export{
    "makeFreeModuleSurjection"
}

beginDocumentation()
undocumented{
    }

-- Main homepage documentation
load "./CpMackeyFunctors/Documentation/MainPageDoc.m2"

-- Documentation for methods and symbols
load "./CpMackeyFunctors/Documentation/ConstructorsDoc.m2"
load "./CpMackeyFunctors/Documentation/HomGroupDoc.m2"
load "./CpMackeyFunctors/Documentation/MackeyFunctorDoc.m2"
load "./CpMackeyFunctors/Documentation/AbelianCategoryDoc.m2"
load "./CpMackeyFunctors/Documentation/HomomorphismsDoc.m2"
load "./CpMackeyFunctors/Documentation/BoxProductDoc.m2"

----------------------------
----------------------------
-- Testing
----------------------------
----------------------------

-- load "./tests/HelpersTest.m2"
-- load "./tests/MackeyFunctorsTest.m2"
