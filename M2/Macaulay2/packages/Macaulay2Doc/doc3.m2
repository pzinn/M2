--		Copyright 1993-1998 by Daniel R. Grayson

document {
     Key => {(symbol /,List,RingElement),(symbol /,List,Number)},
     Headline => "vector division",
     Usage => "v/c",
     Inputs => {"v" => "to be treated as a vector", "c" => "a number or scalar ring element"},
     Outputs => {{ "the quotient vector; every element of ", TT "v", " is divided by ", TT "c" }},
     EXAMPLE "{1,2,3,4} / 3"     
     }

document {
     Key => { (symbol /,VisibleList,Function),
	  (symbol /,List,Function),
	  (symbol \,Function,VisibleList),
	  (symbol \,Function,VirtualTally),
	  (symbol \,SelfInitializingType,VisibleList),
	  (symbol \,Command,VisibleList),
	  (symbol \,RingMap,VisibleList),
	  (symbol \,Command,VirtualTally),
	  (symbol /,VisibleList,SelfInitializingType),
	  (symbol /,List,Command),
	  (symbol /,VirtualTally,Command),
	  (symbol /,VirtualTally,Function),
	  (symbol /,VisibleList,RingMap),
	  (symbol /,VisibleList,Command),
	  (symbol /,String,Command),
	  (symbol /,String,Function),
	  (symbol \,Command,String),
	  (symbol \,Function,String)
	  },
     Headline => "apply a function to elements of a list",
     Usage => "x/f\nf\\x",
     Inputs => { "x" => Nothing => {ofClass{VisibleList,List,Sequence,Array,Tally,Set,String}}, "f" => Nothing => {ofClass{Function,Command,SelfInitializingType,RingMap}} },
     Outputs => {{ "the list, tally, or set obtained by applying ", TT "f", " to each element of ", TT "x", "; it has the same type as ", TT "x", " has" }},
     PARA {
	  "The function ", TO "apply", " does the same thing."
	  },
     PARA {
     	  "The operator ", TO "/", " is left associative, which means that ", TT "w / f / g", " is interpreted as ", TT "(w / f) / g", ".
     	  The operator ", TO "\\", " is right associative, so ", TT ///g \ f \ w///, " is interpreted as ", TT ///g \ (f \ w)///, ".
	  Both operators have parsing precedence lower than that of ", TO "@@", ", which means that the previous two expressions are equivalent to ", TT "w / g @@ f", "
	  and ", TT "g @@ f \\ w", ", respectively. See ", TO "precedence of operators", "."
	  },
     EXAMPLE lines ///
     	  f = x -> x+1
	  g = x -> 2*x
     	  g \ (1 .. 10)
     	  (1 .. 10) / g
     	  f \ g \ (1 .. 10)
     	  f @@ g \ (1 .. 10)
	  set (1 .. 10)
	  g \ oo
	  R = QQ[x];
	  f = map(R,R,{x^2})
	  f \ {x,x^2,x^3,x^4}
     ///,
     SourceCode => {(symbol /,VisibleList,Function)},
     }

document {
     Key => { (symbol /,Ideal,Function),
	  (symbol \,Function,Ideal)},
     Headline => "apply a function to generators of an ideal",
     Usage => "I/f\nf\\I",
     Inputs => { "I","f"},
     Outputs => {List => { "obtained by applying the function ", TT "f", " to each generator of ", TT "I"}},
     PARA {
     	  "The operator ", TO "/", " is left associative, which means that ", TT "w / f / g", " is interpreted as ", TT "(w / f) / g", ".
     	  The operator ", TO "\\", " is right associative, so ", TT ///g \ f \ w///, " is interpreted as ", TT ///g \ (f \ w)///, ".
	  Both operators have parsing precedence lower than that of ", TO "@@", ", which means that the previous two expressions are 
	  equivalent to ", TT "w / g @@ f", "
	  and ", TT "g @@ f \\ w", ", respectively. See ", TO "precedence of operators", "."
	  },
     EXAMPLE lines ///
     	  R = ZZ[a..d];
	  I = ideal"abc-d3,ab-d-1,a2+b2+c3-14d-3"
     	  I/size
	  (f->f+a*b-1)\I
	  I/leadTerm/support/set//sum
     ///,
     }

document {
     Key => {(symbol //,Thing,Function),(symbol \\,Function,Thing),
	  (symbol //,Thing,Command),(symbol \\,Command,Thing),
	  (symbol //,Thing,SelfInitializingType),(symbol \\,SelfInitializingType,Thing)
	  },
     Headline => "apply a function",
     Usage => "x // f\nf \\\\ x",
     Inputs => { "x", "f" => Nothing => {ofClass{Function,Command,SelfInitializingType}}},
     Outputs => {{ "the result of applying ", TT "f", " to ", TT "x", ", i.e., ", TT "f x" }},
     SeeAlso => {(symbol /,VisibleList,Function)},
     PARA {
	  "The parsing precedence of the operators ", TT "//", " and ", TT "\\\\", " is rather low, which makes
	  them useful for avoiding parentheses.  See ", TO "precedence of operators", "."
	  },
     EXAMPLE lines ///
     	  toList \\ sin \ ( 1 .. 5 )
     	  ( 1 .. 5 ) / sin // toList
	  (x -> (x,x)) \ (a,b,c,d)
	  splice \\ (x -> (x,x)) \ (a,b,c,d)
     ///
     }

document { Key => {(numRows, Matrix),(numRows, MutableMatrix),numRows},
     Headline => "number of rows in a matrix or mutable matrix",
     Usage => "numRows m", Inputs => { "m" }, Outputs => {{ "the number of rows in ", TT "m" }}}
document { Key => {(numColumns, Matrix),(numColumns, MutableMatrix),numColumns},
     Headline => "number of columns in a matrix or mutable matrix",
     Usage => "numColumns m", Inputs => { "m" }, Outputs => {{ "the number of columns in ", TT "m" }}}
document { Key => {mutableMatrix,
	  (mutableMatrix, MutableMatrix),
	  (mutableMatrix, Matrix),
	  (mutableMatrix, List),
	  [mutableMatrix, Dense]},
     Headline => "make a mutable matrix",
     Usage => "mutableMatrix m",
     Inputs => { "m" => {ofClass{Matrix, MutableMatrix, List}},
	  Dense => {"whether the encoding of the matrix should be dense or not: see ", TO MutableMatrix}
	  },
     Outputs => {{ "a new mutable matrix whose entries are obtained from ", TT "m", ".  If ", TT "m", " is a list, it should
	       be a doubly nested list (table) of ring elements, all from the same ring." }},
     EXAMPLE lines ///
     	  f = mutableMatrix {{1,2,3,4}}
	  f_(0,2)
	  f_(0,2) = 33
	  f
	  R = QQ[a..z]
	  mutableMatrix genericMatrix(R,3,3)
     ///
     }
document { Key => {(mutableMatrix, Ring, ZZ, ZZ),(mutableMatrix, RingFamily, ZZ, ZZ) },
     Headline => "make a mutable matrix filled with zeroes",
     Usage => "mutableMatrix(R,nrows,ncols)",
     Inputs => { "R",
	          "nrows",
		  "ncols",
	  	  Dense => {"whether the encoding of the matrix should be dense or not: see ", TO MutableMatrix}
		  },
     Outputs => {{"an ", TT "nrows", " by ", TT "ncols", " mutable matrix filled with zeroes from the ring ", TT "R" }},
     EXAMPLE lines ///
         m = mutableMatrix(QQ,10,20)
	 m_(5,5) = 11/13
	 m
     ///,
     SeeAlso => {mutableIdentity, mutableMatrix}
     }
document { Key => {(mutableIdentity, Ring, ZZ),(mutableIdentity, RingFamily, ZZ),
	  [mutableIdentity,Dense],
	  mutableIdentity},
     Headline => "make a mutable identity matrix",
     Usage => "mutableIdentity(R,nrows)",
     Inputs => { "R",
	  "nrows",
	  Dense => {"whether the encoding of the matrix should be dense or not: see ", TO MutableMatrix}
	  },
     Outputs => {
	  MutableMatrix => {"an ", TT "nrows", " by ", TT "nrows", " mutable identity matrix filled with elements of the ring ", TT "R" }},
     EXAMPLE lines ///
         m = mutableIdentity(QQ,10)
	 m_(5,5) = 11/13
	 m
     ///,
     SeeAlso => {mutableMatrix}
     }

document { Key => Bareiss,
     "This symbol is used as one of the permissible values for the strategy option in function dealing with determinants.",
     SeeAlso => {[exteriorPower,Strategy], [minors,Strategy], [det,Strategy]}
     }
document { Key => Cofactor,
     "This symbol is used as one of the permissible values for the strategy option in function dealing with determinants.",
     SeeAlso => {[exteriorPower,Strategy], [minors,Strategy], [det,Strategy]}
     }
document { Key => Dynamic,
     "This symbol is used as one of the permissible values for the strategy option in function dealing with determinants.",
     SeeAlso => {[exteriorPower,Strategy], [minors,Strategy], [det,Strategy]}
     }


document { Key => {(netList, VisibleList),
	  netList,
	  [netList, Boxes],
	  [netList, BaseRow],
	  [netList, HorizontalSpace],
	  [netList, VerticalSpace],
	  [netList, Alignment]},
     Headline => "a table of boxes",
     Usage => "netList v",
     Inputs => {
	  "v" => {"a list of lists of things to be converted to nets and displayed as a table in a net"},
	  Boxes => {"whether to draw boxes around the individual nets.
	      Can be a Boolean, or a pair controlling separately the horizontal and vertical lines of the boxes.
	      Each element of the pair is either a Boolean (draw all or none) or a list of rows/columns where lines are to inserted."},
	  BaseRow => ZZ => {"the index of the base row, for the purpose of setting the baseline of the net produced.  The value
	       is allowed to be as large as the length of ", TT "v", ", larger by 1 than one might expect."},
	  HorizontalSpace => ZZ => {"the amount of space horizontally between entries or between entries and their enclosing boxes"},
	  VerticalSpace => ZZ => "the amount of space vertically between entries or between entries and their enclosing boxes",
	  Alignment => {TT "Center", ", ", TT "Left", ", ", TT "Right", ", or a list of those symbols indicating horizontal adjustment; if it's a list, the ", TT "i", "-th
	       entry specifies the adjustment in the ", TT "i", "-th column; if not, the symbol applies to all columns."}
	  },
     Outputs => {{"a net obtained by converting the elements of each list in the list of lists ", TT "v", " to nets and arranging them in a table, as specified by the
	       options"}},
     EXAMPLE lines ///
	  f = {{"hi there","foo"},{-3, 2^40}}
	  netList f
	  netList(f,Boxes=>false)
	  netList(f,Boxes=>true,HorizontalSpace=>1,VerticalSpace=>1)
	  netList(f,Boxes=>true,Alignment=>Center)
	  netList(f,Boxes=>true,BaseRow=>1)
	  netList(f,Boxes=>{{1},{1}})
	  netList apply(5,i->apply(i+1,j->(i,j)))
	  netList(apply(5,i->apply(i+1,j->(i,j))),Boxes=>{true,false})
     ///}

document { Key => centerString,
     Headline => "center a string or net",
     Usage => "centerString(wid,s)",
     Inputs => { "wid" => ZZ, "s" => Net },
     Outputs => {{"a net with spaces added, as needed, to center ", TT "s", " in a net of width ", TT "wid" }},
     EXAMPLE lines ///
         centerString(18,"asdf"||"qwer")
     ///}

document { Key => {(rotate, ZZ, VisibleList),rotate},
     Headline => "rotate a list",
     Usage => "rotate(n,v)",
     Inputs => {"n","v"},
     Outputs => {{"the list obtained by rotating the list ", TT "v", " leftward ", TT "n", " places"}},
     EXAMPLE lines ///
     	 p = 0 .. 20
	 rotate(3,p)
	 rotate(-3,p)
     ///}


document { Key => info,
     Headline => "convert hypertext to info format",
     "This function is used internally when preparing documentation."
     }
document { Key => pager,
     Headline => "display with paging",
     Usage => "pager x",
     Inputs => {"x"},
     Consequences => {{TT "x", " is converted to a net and displayed through the pager specified by the environment variable PAGER, if set,
	       else through the program ", TT "more", "."
	       }}}

document { Key => {precision,
	  (precision, GaloisField), (precision, FractionField),
	  (precision, QuotientRing), (precision, Ring),(precision,Number),
	  (precision, MutableMatrix),(precision, RingElement),(precision, PolynomialRing),
	  (precision, InexactNumber),(precision, InexactField),(precision, Matrix)
	  },
     Usage => "precision x",
     Inputs => { "x" => {ofClass{Ring,Matrix,RingElement,Number}}},
     Outputs => { ZZ => {"the precision to which ", TT "x", " or its instances are stored"}},
     EXAMPLE lines ///
     	  precision 3p111
	  precision (RR[x])
	  precision 3
     ///
     }

document { Key => "printingTimeLimit",
     "This variable specifies the number of seconds to allow for printing an output line"
     }
document { Key => "printingPrecision",
     Headline => "current precision for printing numbers",
     Usage => "printingPrecision = n",
     Inputs => {
	  "n" => ZZ
	  },
     Consequences => {
	  {"Henceforth, inexact numbers are printed with at most ", TT "n", " digits of precision.
	       Meaningless digits will not be displayed.
	       The special case where ", TT "n=0", " is
	       interpreted as meaning ", TT "n=infinity", ", and this case is
	       used when a number appears alone on an output line to display
	       all the meaningful digits."}
	  },
     EXAMPLE lines ///
     	  1/3p100
     	  {1/3p100}
	  printingPrecision
	  printingPrecision = 16
     	  {1/3p100}
	  printingPrecision = 0
     	  {1/3p100}
     ///,
     PARA {
	  "For complex numbers, if ", TO "printingAccuracy", " is set to its default value of ", TT "-1", ",
	  the two parts of the number are treated together (although a digit further further to the right of
	  the point may sometimes be displayed in the smaller part)."
	  },
     EXAMPLE lines ///
     printingAccuracy
     printingPrecision = 16
     {1p100e12/3+1p100/3*ii}
     printingAccuracy = 10
     {1p100e12/3+1p100/3*ii}
     ///,
     SeeAlso => {"printingAccuracy", "printingLeadLimit", "printingTrailLimit", "printingSeparator", format}
     }
document { Key => "printingAccuracy",
     Headline => "current accuracy for printing numbers",
     Usage => "printingAccuracy = n",
     Inputs => {
	  "n" => ZZ
	  },
     Consequences => {
	  {"Henceforth, inexact numbers are printed with at most ", TT "n", " digits to the right of
	       the decimal point displayed.
	       The special case where ", TT "n=-1", " is
	       interpreted as meaning ", TT "n=infinity", ", and this case is
	       used when a number appears alone on an output line."}
	  },
     EXAMPLE lines ///
	  printingPrecision,printingAccuracy
     	  1p100e-5/3
     	  x = {1p100e-5/3,1p100e-4/3,1p100e-3/3,1p100e-2/3}
	  printingAccuracy = 8
     	  x
	  printingAccuracy = 4
     	  x
     ///,
     SeeAlso => {"printingAccuracy", "printingLeadLimit", "printingTrailLimit", "printingSeparator", format}
     }
document { Key => "printingLeadLimit",
     Headline => "maximum number of leading zeroes to use when printing real numbers",
     Usage => "printingLeadLimit = n",
     Inputs => {
	  "n" => ZZ
	  },
     Consequences => {
	  {"Real numbers are printed with at most ", TT "n", " leading zeroes."}
	  },
     EXAMPLE lines ///
     	  1/30000000000.
	  printingLeadLimit
	  printingLeadLimit = 20
     	  1/30000000000.
     ///,
     SeeAlso => {"printingPrecision", "printingAccuracy", "printingTrailLimit", "printingSeparator", format}
     }
document { Key => "printingTrailLimit",
     Headline => "maximum number of additional trailing digits to use when printing real numbers",
     Usage => "printingTrailLimit = n",
     Inputs => {
	  "n" => ZZ
	  },
     Consequences => {
	  {"Real numbers are printed with at most ", TT "n", " additional trailing digits, in addition to those specified by ", TT "printingPrecision", "."}
	  },
     EXAMPLE lines ///
     	  3000000000000.
	  printingTrailLimit
	  printingTrailLimit = 20
     	  3000000000000.
     ///,
     SeeAlso => {"printingPrecision", "printingAccuracy", "printingLeadLimit", "printingSeparator", format}
     }
document { Key => "printingSeparator",
     Headline => "string used to separate mantissa from exponent when printing real numbers",
     Usage => "printingSeparator = s",
     Inputs => {
	  "s" => String
	  },
     Consequences => {
	  {"The string ", TT "s", " will be used to separate mantissa and exponent when printing real numbers."}
	  },
     EXAMPLE lines ///
     	  3000000000000.
	  printingSeparator
	  printingSeparator = "E"
     	  3000000000000.
     ///,
     SeeAlso => {"printingPrecision", "printingAccuracy", "printingLeadLimit", "printingTrailLimit", format}
     }

-- Local Variables:
-- compile-command: "make -C $M2BUILDDIR/Macaulay2/m2 "
-- End:
