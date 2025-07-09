--CpMackeyFunctors.m2
newPackage(
    "CpMackeyFunctors",
    Version=>"1.0",
    Date=>"July 4, 2025",
    Authors=>{
        {Name=>"Thomas Brazelton",
            Email=>"brazelton@math.harvard.edu",
            HomePage=>"https://tbrazel.github.io/"},
        {Name=>"David Chan",
            Email=>"chandav2@msu.edu",
            HomePage=>"https://sites.google.com/view/davidchanmath"},
        {Name=>"Benjamin Mudrak",
            Email=>"bmudrak@purdue.edu",
            HomePage=>"https://benjaminmudrak.github.io"},
        {Name=>"Ben Spitz",
            Email=>"benspitz@virginia.edu",
            HomePage=>"https://benspitz.com/"},
        {Name=>"Chase Vogeli",
            Email=>"cpv29@cornell.edu",
            HomePage=>"https://chasevoge.li"},
        {Name=>"Chenglu Wang",
            Email=>"cwang@math.harvard.edu",
            HomePage=>"https://chengluw.github.io"},
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
    PackageImports=>{"OldChainComplexes"},
    -- OldChainComplexes needed to overload `res`
    -- One day: export Complexes and use that framework for computing derived functors
    PackageExports=>{},
    AuxiliaryFiles => true,
    DebuggingMode=>true
)

importFrom_Core {
    "concatBlocks", "concatCols", "concatRows",
    "isMorphism", "isAbelianCategory",
    }

-- Code for the CpMackeyFunctor type
load "./CpMackeyFunctors/Code/MackeyFunctor.m2"
export{
    "CpMackeyFunctor",
    "makeCpMackeyFunctor",
    "PrimeOrder",
    "Res",
    "Trans",
    "Conj",
    "Underlying",
    "Fixed",
    "isCohomological",
    "drawVerticalCpMackeyFunctor"
}

-- Constructor files for making various Mackey functors
load "./CpMackeyFunctors/Code/Constructors.m2"
export{
    "makeBurnsideMackeyFunctor",
    "makeUnderlyingFreeMackeyFunctor",
    "makeComplexRepresentationMackeyFunctor",
    "makeRealRepresentationMackeyFunctor",
    "makeZeroMackeyFunctor",
    "makeFixedPointMackeyFunctor",
    "makeOrbitMackeyFunctor",
    "makeFixedTrivMackeyFunctor",
    "makeFixedSignMackeyFunctor",
    "makeZeroOnUnderlyingMackeyFunctor",
    "makeKGroupMackeyFunctor"
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
    "isTrivialMackeyFunctor",
}

-- Methods for working in the abelian category Mack_p.
-- Note that ++ and ker and coker are already methods, hence exported
load "./CpMackeyFunctors/Code/AbelianCategory.m2"

-- Methods for forming the abelian group of homomorphisms between two Mackey functors
-- Similarly Hom is already an exported method
load "./CpMackeyFunctors/Code/HomGroup.m2"
load "./CpMackeyFunctors/Code/InternalHom.m2"
export {
    "InternalHom"
}

load "./CpMackeyFunctors/Code/BoxProduct.m2"
export{
    "boxProduct"
}

load "./CpMackeyFunctors/Code/Resolutions.m2"

load "./CpMackeyFunctors/Code/HomAlg.m2"

load "./CpMackeyFunctors/Code/RandomMackeyFunctor.m2"
export{
    "makeRandomCpMackeyFunctor",
    "GenBound",
    "makeRandomMackeyFunctorHomomorphism",
}

load "./CpMackeyFunctors/Code/Cohomological.m2"
export{
    "resolutionCohomological",
    "TorCoh",
    "ExtCoh"
}

beginDocumentation()
undocumented{
    (isWellDefined,CpMackeyFunctor),
    (isWellDefined,MackeyFunctorHomomorphism),
    (ring, MackeyFunctorHomomorphism),
    (promote, MackeyFunctorHomomorphism, ZZ)
    }

-- Main homepage documentation
load "./CpMackeyFunctors/Documentation/MainPageDoc.m2"

-- Mathematical background
load "./CpMackeyFunctors/Documentation/BackgroundDoc.m2"

-- List of common Mackey functors
load "./CpMackeyFunctors/Documentation/ListOfCommonMFsDoc.m2"

-- Worked-out applications
load "./CpMackeyFunctors/Documentation/ApplicationsDoc.m2"


-- Documentation for methods and symbols
load "./CpMackeyFunctors/Documentation/ConstructorsDoc.m2"
load "./CpMackeyFunctors/Documentation/HomGroupDoc.m2"
load "./CpMackeyFunctors/Documentation/MackeyFunctorDoc.m2"
load "./CpMackeyFunctors/Documentation/AbelianCategoryDoc.m2"
load "./CpMackeyFunctors/Documentation/HomomorphismsDoc.m2"
load "./CpMackeyFunctors/Documentation/BoxProductDoc.m2"
load "./CpMackeyFunctors/Documentation/RandomMackeyFunctorDoc.m2"
load "./CpMackeyFunctors/Documentation/InternalHomDoc.m2"
load "./CpMackeyFunctors/Documentation/HomAlgDoc.m2"
load "./CpMackeyFunctors/Documentation/ResolutionsDoc.m2"

----------------------------
----------------------------
-- Testing
----------------------------
----------------------------

-- load "./tests/HelpersTest.m2"
-- load "./tests/MackeyFunctorsTest.m2"
--load "./tests/CohomologicalTest.m2"
