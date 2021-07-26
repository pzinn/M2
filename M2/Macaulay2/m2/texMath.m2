-- in the new version, all types here should have expression XXX := hold

texUnder := (x,y) -> "\\underset{\\vphantom{\\Bigg|}"|y|"}{"|x|"}"

union := (x,y) -> keys(set x + set y)
intersection := (x,y) -> keys(set x * set y)

---

-- texMath' should always be used rather than texMath unless absolutely certain that no descendant type needs redefining texMath'

texMath' = method()

---

-- this is almost but not quite expressionifiable
texMath' (Function, GradedModuleMap) := (texMath,f) -> (
     d := f.degree;
     s := sort intersection(spots f.source, spots f.target / (i -> i - d));
     texMath if #s === 0 then ZERO else new VerticalList from apply(s,i-> RowExpression {i+d, ":", MapExpression { target f_i, source f_i, f_i }, ":", i})
)


texMath' (Function, GradedModule) := (texMath,C) -> (
     s := sort spots C;
     if # s === 0 then "0"
     else demark("\\quad ",apply(s,i->texUnder(texMath C_i,i)))
      )




texMath' (Function, ChainComplex) := (texMath,C) -> (
     complete C;
     s := sort spots C;
     if # s === 0 then "0" else
     concatenate apply(s,i->(
	     if i>s#0 then "\\,\\xleftarrow{" | texMath short C.dd_i | "}\\,",
	     texUnder(texMath C_i,i)
	     ))
      )

texMath BettiTally := v -> (
     v = rawBettiTally v;
     concatenate(
	  "\\begin{matrix}\n",
	  apply(v, row -> (between("&", apply(row,x->if not match("^[0-9]*$",x) then ("\\text{",x,"}") else x)), "\\\\")),
	  "\\end{matrix}\n",
	  ))

texMath' (Function, Descent) := (texMath,x) -> "\\left|\\begin{array}{l}" | concatenate sort apply(pairs x,
     (k,v) -> (
	  if #v === 0
	  then texMath net k -- sucks but no choice
	  else texMath net k | " : " | texMath v
	  ) | "\\\\") | "\\end{array}\\right."

texMath' (Function, Parenthesize) := (texMath,x) -> texMath x#0

texMath' (Function, RowExpression) := (texMath,w) -> concatenate apply(w,texMath)

--texMath' (Function, Keyword) := (texMath,x) -> if keywordTexMath#?x then keywordTexMath#x else texMath toString x

spacedOps := set { symbol =>, symbol and, symbol or, symbol xor, symbol ++ }

texMath' (Function, BinaryOperation) := (texMath,m) -> (
     x := texMath m#1;
     y := texMath m#2;
     if rightPrecedence m#1 < lprec m#0 then x = "\\left(" | x | "\\right)";
     if precedence m#2 <= rprec m#0 then y = "\\left(" | y | "\\right)";
     if spacedOps#?(m#0) then concatenate( x, "\\ ", texMath m#0, "\\ ", y ) else concatenate( x, texMath m#0, y )
     )

texMath' (Function, NewFromExpression) := -- for now
texMath' (Function, Adjacent) := texMath' (Function, FunctionApplication) := (texMath,m) -> (
     p := precedence m;
     fun := m#0;
     args := m#1;
     div := instance(fun,Divide);
     pfun := if div then strength1 symbol symbol else precedence fun;
     -- we can finesse further the amount of space than in net
     -- see also https://tex.stackexchange.com/questions/2607/spacing-around-left-and-right
     if div or instance(args,Array) then sep:="\\mathopen{}"
     else if instance(args,VisibleList) then sep="{}"
     else sep = "\\ ";
     if precedence args > p
     then if pfun >= p
     then concatenate (texMath fun, sep, texMath args)
     else concatenate ("\\left(", texMath fun, "\\right)", texMath args)
     else if pfun >= p
     then concatenate (texMath fun, "\\left(", texMath args, "\\right)")
     else concatenate ("\\left(",texMath fun,"\\right)\\left(", texMath args, "\\right)")
     )


texMath' (Function, Expression) := (texMath,v) -> (
     op := class v;
     p := precedence v;
     names := apply(toList v,term -> (
	       if precedence term <= p
	       then ("{\\left(", texMath term, "\\right)}")
	       else ("{", texMath term, "}") ) );
     if # v === 0 then (
	  if op.?EmptyName then op.EmptyName
	  else error("no method for texMath ", op)
	  )
     else (
	  if op.?operator then demark(op.operator,names)
	  else error("no method for texMath ", op)
	  )
     )

texMath' (Function, Minus) := (texMath,v) -> (
     term := v#0;
     if precedence term <= precedence v
     then "-\\left(" | texMath term | "\\right)"
     else "-" | texMath term
     )

texMath' (Function, Divide) := (texMath,x) -> "\\frac{" | texMath x#0 | "}{" | texMath x#1 | "}"

texMath' (Function, Sum)  := (texMath,v) -> (
     n := # v;
     if n === 0 then "0"
     else (
	  p := precedence v;
	  seps := apply(toList(1..n-1), i -> if class v#i === Minus then "" else "+");
	  names := apply(n, i -> (
		    if precedence v#i <= p and class v#i =!= Minus
		    then "\\left(" | texMath v#i | "\\right)"
		    else texMath v#i ));
	  concatenate mingle ( names, seps )))

texMath' (Function, Product) := (texMath,v) -> (
     n := # v;
     if n === 0 then "1"
     else if n === 1 then texMath v#0
     else (
     	  p := precedence v;
	  nums := apply(v, x -> isNumber x or (class x === Power and isNumber x#0 and (x#1 === 1 or x#1 === ONE)));
	  seps := apply (n-1, i-> if nums#i and (nums#(i+1) or class v#(i+1) === Power and isNumber v#(i+1)#0) then "\\cdot " else if nums#i or class v#i === Symbol or (class v#i === Power and class v#i#0 === Symbol and (v#i#1 === 1 or v#i#1 === ONE)) then "\\," else "");
     	  boxes := apply(v,
		    term -> (
			 if precedence term <= p and class expression term =!= Divide
			 then "\\left(" | texMath term | "\\right)"
			 else texMath term
			 )
		    );
	  concatenate splice mingle (boxes,seps)
	  )
      )

texMath' (Function, Power) := (texMath,v) -> if v#1 === 1 or v#1 === ONE then texMath v#0 else (
    p := precedence v;
    x := texMath v#0;
    y := texMath v#1;
    if precedence v#0 <  p then x = "\\left({" | x | "}\\right)";
    concatenate(x,"^{",y,"}") -- no braces around x
    )

texMath' (Function, Superscript) := (texMath,v) -> if v#1 === moduleZERO then "0" else (
    p := precedence v;
    x := texMath v#0;
    y := texMath v#1;
    if precedence v#0 <  p then x = "\\left({" | x | "}\\right)";
    concatenate(x,"^{",y,"}") -- no braces around x
    )

texMath' (Function, Subscript) := (texMath,v) -> (
     p := precedence v;
     x := texMath v#0;
     if class v#1 === Sequence then y:=demark(",", apply(v#1,texMath)) else y = texMath v#1; -- no parentheses
     if precedence v#0 <  p then x = "\\left(" | x | "\\right)";
     concatenate(x,"_{",y,"}")
     )

texMath' (Function, SparseVectorExpression) := (texMath,v) -> (
     n := v#0;
     w := newClass(MutableList, apply(n,i->"0"));
     scan(v#1,(i,x)->w#i=texMath x);
     concatenate("\\begin{pmatrix}", between("\\\\ ",w),"\\end{pmatrix}")
     )

texMath' (Function, SparseMonomialVectorExpression) := (texMath,v) -> (
     texMath sum(v#1,(i,m,a) ->
	  expression a *
	  expression m *
	  hold concatenate("<",toString i,">"))
     )

texMath' (Function, VerticalList) := (texMath,s) -> concatenate(
    "\\left\\{\\begin{aligned}",
    between("\\\\",apply(toList s,x->"&"|texMath x))
    ,"\\end{aligned}\\right\\}"
    )

texMath' (Function, NumberedVerticalList) := (texMath,s) -> concatenate(
    "\\left\\{\\begin{aligned}",
    between("\\\\",apply(#s,i->i|".\\quad&"|texMath s#i))
    ,"\\end{aligned}\\right\\}"
    )

texMath' (Function, Table) := (texMath,m) -> (
    if m#?0 then concatenate(
	"{\\begin{array}{", #m#0: "c", "}", newline,
	apply(m, row -> (between("&",apply(row,texMath)), ///\\///|newline)),
	"\\end{array}}")
)

matrixOpts := x -> ( -- helper function
    opts := hashTable{CompactMatrix=>compactMatrixForm,BlockMatrix=>null,Degrees=>null,mutable=>false};
    (opts,x) = override(opts,toSequence x);
    if class x === Sequence then x = toList x else if #x === 0 or class x#0 =!= List then x = { x }; -- for backwards compatibility
    (opts,x)
    )

texMath' (Function, MatrixExpression) := (texMath,x) -> (
    (opts,m) := matrixOpts x;
    if all(m,r->all(r,i->class i===ZeroExpression)) then return "0";
    if opts.BlockMatrix =!= null then ( j := 1; h := 0; );
--    texMath1 := if opts.CompactMatrix then texMath else x -> "\\displaystyle "|texMath x;
    m = applyTable(m,texMath);
    concatenate(
	if opts.Degrees =!= null then (
	    degs := apply(opts.Degrees#0,texMath);
	    if opts.CompactMatrix then "\\begin{smallmatrix}" else "\\begin{array}{l}",
	    apply(#m, i -> degs#i | "\\vphantom{" | concatenate m#i | "}\\\\"),
	    if opts.CompactMatrix then "\\end{smallmatrix}" else "\\end{array}"
	    ),
	"\\left(",
	if opts.CompactMatrix then "\\begin{smallmatrix}" else {
	    "\\!\\begin{array}{",
	    if opts.BlockMatrix =!= null then demark("|",apply(opts.BlockMatrix#1,i->i:"c")) else #m#0:"c",
	    "}"
	    },
	newline,
	apply(#m, i -> concatenate(
		if opts.Degrees =!= null then "\\vphantom{"| degs#i | "}",
		 between("&",m#i),
		 "\\\\",
		 newline,
		 if opts.BlockMatrix =!= null then if h<#opts.BlockMatrix#0-1 and j == opts.BlockMatrix#0#h then (j=0; h=h+1; "\\hline\n") else (j=j+1;)
		 )),
	if opts.CompactMatrix then "\\end{smallmatrix}" else "\\end{array}\\!",
	"\\right)"
	)
)

texMath' (Function, VectorExpression) := (texMath,v) -> (
    concatenate(
	"\\left(",
	if compactMatrixForm then "\\begin{smallmatrix}" else "\\!\\begin{array}{c}",
	newline,
	between(///\\///,apply(toList v,texMath)),
	if compactMatrixForm then "\\end{smallmatrix}" else "\\end{array}\\!",
	"\\right)"
	)
    )

texMath' (Function, RR) := (texMath,x) -> if not isANumber x then texMath toString x else if isInfinite x then if x>0 then texMath infinity else texMath (-infinity) else "{"|format(printingPrecision,printingAccuracy,printingLeadLimit,printingTrailLimit,"}\\cdot 10^{",x)|"}"

texMath ZZ := toString -- eventually, change
tex Thing := x -> concatenate("$",texMath x,"$")

texMathTable = new HashTable from {
    symbol |- => "\\vdash ",
    symbol .. => "\\,{.}{.}\\, ",
    symbol ..< => "\\,{.}{.}{<}\\, ",
    symbol <= => "\\le ",
    symbol >= => "\\ge ",
    symbol => => "\\Rightarrow ",
    symbol ==> => "\\Longrightarrow ",
    symbol <== => "\\Longleftarrow ",
    symbol <==> => "\\Longleftrightarrow ",
    symbol ** => "\\otimes ",
    symbol ++ => "\\oplus ",
    symbol != => "\\ne ",
    symbol -> => "\\rightarrow ",
    symbol <- => "\\leftarrow ",
    symbol ===> => "{\\large\\Longrightarrow}",
    symbol <=== => "{\\large\\Longleftarrow}",
    symbol << => "\\ll ",
    symbol >> => "\\gg ",
    symbol ~ => "\\sim ",
    symbol ^** => "^{\\otimes}",
    symbol _ => "\\_ ",
    symbol | => "|",
    symbol || => "||",
    symbol { => "\\{ ",
    symbol } => "\\} ",
    symbol \ => "\\backslash ",
    symbol \\ => "\\backslash\\backslash ",
    symbol SPACE => "\\texttt{SPACE}",
    symbol # => "\\#",
    symbol #? => "\\#?",
    symbol % => "\\%",
    symbol & => "\\&",
    symbol ^ => "\\wedge",
    symbol ^^ => "\\wedge\\wedge",
    pi => "\\pi ",
    EulerConstant => "\\gamma ",
    ii => "\\mathbf{i}",
    OO => "\\mathcal{O}",
    symbol <| => "\\langle",
    symbol |> => "\\rangle"
    }

-- experimental change (same with toString, net, etc)
texMath Thing := v -> texMath'(texMath,v)

texMath' (Function, Thing) := (texMath1,x) -> if texMathTable#?x then texMathTable#x else if lookup(texMath,class x) =!= Thing#texMath then texMath x else (
    -- annoying extra test in case texMath redefined directly. e.g. currently: texMath ZZ
    y := expression x;
    -- we need to avoid loops: objects whose expression is a Holder and whose texMath is undefined
    -- we could have a stricter if lookup(expression,class x) === hold but that might be too restrictive
    if instance(y,Holder) and class y#0 === class x then texMath1 toString x -- if we're desperate (in particular, for raw objects)
    else texMath1 y )
texMath' (Function, Holder) := (texMath,x) -> if #x === 0 then "" else texMath x#0
--texMath Symbol := toString -- the simplest version
-- next version is a horrible hack, just kept to remind me that:
-- expression should never need to run value of course !!!
--texMath Symbol := x -> ( xx := value x; if instance(xx,HashTable) and xx.?texMath then xx.texMath else toString x)
bbLetters := set characters "kABCDEFGHIJKLMNOPQRSTUVWXYZ"
suffixes := {"bar","tilde","hat","vec","dot","ddot","check","acute","grave","breve"};
suffixesRegExp := "("|demark("|",suffixes)|")\\'";
texVariable := x -> (
    if x === "" then return "";
    xx := separate("\\$",x); if #xx > 1 then return concatenate between("\\$",texVariable\xx);
    if #x === 2 and x#0 === x#1 and bbLetters#?(x#0) then return "{\\mathbb "|x#0|"}";
    if #x>4 and substring(x,0,4) === "sqrt" then return "\\sqrt{"|texVariable substring(x,4)|"}";
    if last x === "'" then return texVariable substring(x,0,#x-1) | "'";
    r := regex(suffixesRegExp,x); if r =!= null then (
	r = first r;
	return "\\"|substring(r,x)|"{"|texVariable substring(x,0,r#0)|"}"
	);
    if #x === 1 or regex("[^[:alnum:]]",x) =!= null then x else "\\textit{"|x|"}"
    )
--texMath' (Function, Symbol) := (texMath,x) -> if texMathTable#?x then texMathTable#x else texVariable toString x;
texMath Symbol := x -> if texMathTable#?x then texMathTable#x else texVariable toString x;

texMath' (Function, SheafExpression) := (texMath,x) -> texMath x#0

texMath' (Function, MapExpression) := (texMath,x) -> texMath x#0 | "\\," | (if #x>2 then "\\xleftarrow{" | texMath x#2 | "}" else "\\longleftarrow ") | "\\," | texMath x#1

texMath' (Function, List) := (texMath,x) -> concatenate("\\left\\{", between(",\\,", apply(x,texMath)), "\\right\\}")
texMath' (Function, Array) := (texMath,x) -> concatenate("\\left[", between(",", apply(x,texMath)), "\\right]")
texMath' (Function, Sequence) := (texMath,x) -> concatenate("\\left(", between(",", apply(x,texMath)), "\\right)")
--texMath' (Function, HashTable) := (texMath,x) -> if x.?texMath then x.texMath else (lookup(texMath',Function,Thing)) (texMath,x)

texMath' (Function, Function) := (texMath,x) -> texMath toString x
texMath' (Function, MutableList) := (texMath,x) -> concatenate (
    texMath class x,
    "\\left\\{",
    if #x > 0 then "\\ldots "|#x|"\\ldots ",
    ,"\\right\\}"
    )

texMath String := s -> "\\texttt{" | texLiteral s | "}"
-- this truncates very big nets
maxlen := 3000; -- randomly chosen
texMath Net := n -> (
    dep := depth n; hgt := height n;
    s:="";
    len:=0; i:=0;
    scan(unstack n, x->(
	    i=i+1;
	    len=len+#x;
	    if i<#n and len>maxlen then (
		s=s|"\\vdots\\\\"|"\\vphantom{\\big|}" | texMath last n | "\\\\";
		if i<hgt then (hgt=i; dep=1) else dep=i+1-hgt;
		break
		);
	    s=s|"&\\vphantom{\\big|}" | texMath x | "\n";
	    if i<#n then s=s|"\\\\[-1mm]";
	    ));
    "\\begin{aligned}" | s | "\\end{aligned}"
    )

texMath' (Function, ColumnExpression) := (texMath,x) -> concatenate (
    "\\begin{aligned}",
    apply(toList x,y -> "&" | texMath y | "\\\\"), -- kinda works
    "\\end{aligned}"
    )

texMath' (Function, Bag) := (texMath,x) -> concatenate(
    texMath class x,
     "\\{",
     if #x>0 then toString(#x) | "\\text{ items}",
     "\\}"
     )

texMath' (Function, Package) :=
texMath' (Function, GroebnerBasis) :=
texMath' (Function, IndeterminateNumber) := (texMath,x) -> texMath toString x

texMath InfiniteNumber := i -> if i === infinity then "\\infty" else "{-\\infty}"

texMath (Function, SumOfTwists) := (texMath,S) -> texMath S#0 | if S#1#0 === neginfinity then "(*)" else "(\\ge" | texMath S#1#0 | ")"

-- MutableHashTable needs manual output... annoying that need to check texMathTable just for OO
texMath' (Function, MutableHashTable) := (texMath,x) -> if texMathTable#?x then texMathTable#x else if hasAttribute(x,ReverseDictionary) then texMath simpleToString getAttribute(x,ReverseDictionary) else concatenate (
    texMath class x,
    "\\left\\{",
    if #x>0 then {"\\ldots",texMath(#x),"\\ldots"},
    "\\right\\}"
)

--- ... but not some of its descendants
texMath' (Function, Ring) :=
texMath' (Function, RingFamily) :=
texMath' (Function, Variety) :=
lookup(texMath',Function,Thing)

texMath' (Function, GroebnerBasis) := (texMath,x) -> texMath toString x

texMath InfiniteNumber := i -> if i === infinity then "\\infty" else "{-\\infty}"

texMath (Function, SumOfTwists) := (texMath,S) -> texMath S#0 | if S#1#0 === neginfinity then "(*)" else "(\\ge" | texMath S#1#0 | ")"
