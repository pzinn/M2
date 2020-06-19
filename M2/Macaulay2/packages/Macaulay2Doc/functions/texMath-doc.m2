--- status: DRAFT
--- author(s): from before
--- notes: 

undocumented {
    (texMath,BettiTally),
    (texMath,ColoredExpression), 
    (texMath,Function,SumOfTwists),
    (texMath,InfiniteNumber),
    (texMath,Net),
    (texMath,Nothing),
    (texMath,String),
    (texMath,Thing),
    (texMath,ZZ)
    }

document { 
     Key => texMath,
     Headline => "convert to TeX math format",
     Usage => "texMath x",
     Inputs => {
	  "x" => "any Macaulay2 object"
	  },
     Outputs => {
	  String => {TT "x", " converted to TeX math format"}
	  },
     "The main difference between this and ", TO tex, " is that the
     surrouding dollar signs aren't there.",
     EXAMPLE {
	  "R = ZZ[x];",
      	  "texMath (x-1)^6",
	  },
     Caveat => {
	  "No attempt is made to wrap large matrices or equations."
	  },
     SeeAlso => {tex, mathML, showTex}
     }
