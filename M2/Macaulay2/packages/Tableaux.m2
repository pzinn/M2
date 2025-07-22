newPackage(
    "Tableaux",
    Version => "1.0",
    Date => "July 22, 2025",
    Authors => {
	{Name => "John Graf", Email => "jrgraf@alumni.ncsu.edu", HomePage => "https://j-graf.github.io/"}},
    Headline => "a package for constructing skew tableaux",
    Keywords => {"Combinatorics"},
    AuxiliaryFiles => true,
    DebuggingMode => false,
    PackageImports => {"Permutations"}--,
    --Configuration => {"Convention" => "English", "TexPackage" => "aTableau"}
    )

export {"SkewTableau", "skewTableau",
        "youngDiagram", "ferrersDiagram", "drawInnerShape",
        "skewShape", "standardize", "rowEntries", "columnEntries", "rowRange", "columnRange",
        "isSkew", "isWeaklyDecreasing", "isNonnegative",
        "toPosition", "toIndex", "positionList", "applyEntries", "applyPositions",
        "verticalConcatenate", "shift", "unshift",
        "boxContent", "hookLength",
        }

export {"YngTableau", "yngTableau",
        "shape"}

export {"allSemistandardTableaux", "numSemistandardTableaux"}


    
-- Implimentation of class SkewTableau

load "Tableaux/SkewTableaux.m2"



-- Implimentation of subclass YngTableau

load "Tableaux/YngTableaux.m2"



-- TODO: Implimentation of subclass Tabloid

-- load "Tableaux/Tabloids.m2"



-- Algorithms involving tableaux

load "Tableaux/algorithms.m2"



-- Documentation

beginDocumentation()

load "Tableaux/documentation.m2"



-- Tests

load "Tableaux/tests.m2"



end--
