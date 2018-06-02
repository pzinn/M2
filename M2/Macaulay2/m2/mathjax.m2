-- some texMath that got stranded
texMath BasicList := s -> concatenate(
     if class s =!= List then texMath class s,
    "\\left\\{",
    between(",\\,",apply(toList s,texMath))
    ,"\\right\\}"
    )
texMath Array := x -> concatenate("\\left[", between(",", apply(x,texMath)), "\\right]")
texMath Sequence := x -> concatenate("\\left(", between(",", apply(x,texMath)), "\\right)")
texMath HashTable := x -> if x.?texMath then x.texMath else
if hasAttribute(x,ReverseDictionary) then texMath toString getAttribute(x,ReverseDictionary) else
concatenate flatten (
    texMath class x,
    "\\left\\{",
    if mutable x then if #x>0 then {"\\ldots",texMath(#x),"\\ldots"} else "" else
    between(",\\,", apply(sortByName pairs x,(k,v) -> texMath k | "\\,\\Rightarrow\\," | texMath v)),
    "\\right\\}"
    )
texMath MonoidElement := x -> texMath expression x
texMath Function := x -> texMath toString x

-- strings -- compare with hypertext.m2
-*
texVerbLiteralTable := new MutableHashTable
    scan(characters ascii(0 .. 255), c -> texVerbLiteralTable#c = c)
    texVerbLiteralTable#"!" = ///!\texttt{!}\verb!///
    --texVerbLiteralTable#"$" = ///!\texttt{\$}\verb!/// -- eww ugly fix of #375 of mathJax. not needed if not enclosing using $
    texVerbLiteralTable#"\\"= ///!\verb!\!\verb!/// -- eww ugly fix of #375 of mathJax
    -- unfortunately the next 2 (needed if the string happens to be in a {} group) may result in wrong font in normal LaTeX depending on encoding, see https://stackoverflow.com/questions/2339651/how-to-get-real-braces-in-ttfont-in-latex
    texVerbLiteralTable#"{" =///!\texttt{\{}\verb!/// -- eww ugly fix of #375 of mathJax
    texVerbLiteralTable#"}" =///!\texttt{\}}\verb!/// -- eww ugly fix of #375 of mathJax
texVerbLiteral = s -> concatenate apply(characters s, c -> texVerbLiteralTable#c)
--texMath String := s -> "\\verb|"|texVerbLiteral s|"|"

texMath String := s -> (
    ss := separate s;
    if #ss <=1 then replace(///\\verb!!///,"",///\verb!///|texVerbLiteral s|///!///) -- to optimize compilation
    else texMath stack ss
    )
*-

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
		s=s|"\\vdots\\\\"|"\\vphantom{\\big|}" | texMath last n | "\\\\[-1mm]";
		if i<hgt then (hgt=i; dep=1) else dep=i+1-hgt;
		break
		);
	    s=s|"\\vphantom{\\big|}" | texMath x | "\n";
	    if i<#n then s=s|"\\\\[-1mm]";
	    ));
--    "\\raise"|toString (2.15*(-dep+hgt-1))|"mm"| -- 2.65 for [-2mm]. this number may have to be adjusted/defined more properly, disabling for now
    "\\begin{array}{l}" | s | "\\end{array}"
    )

texMath ColumnExpression := x -> concatenate (
    "\\begin{array}{l}",
    apply(toList x,y -> texMath y |"\\\\"), -- kinda works
    "\\end{array}"
    )

-- now the mathJax stuff per se
-- mathJax Thing produces some valid html code with possible tex code in \( \)
-- topLevelMode=MathJax produces that plus possible pure text coming from the system
-- hence, requires tags to help the browser app distinguish html from text
(mathJaxEndTag,            -- closing tag
    mathJaxHtmlTag,        -- indicates what follows is HTML
    mathJaxOutputTag,      -- it's html but it's output
    mathJaxInputTag,       -- it's text but it's input
    mathJaxInputContdTag,  -- text, continuation of input
    mathJaxTextTag):=      -- other text
apply((17,18,19,20,28,30),ascii)

texAltLiteralTable := hashTable { "$" => "\\$", "\\" => "\\verb|\\|", "{" => "\\{", "}" => "\\}",
    "&" => "\\&", "^" => "\\verb|^|", "_" => "\\_", " " => "\\ ", "%" => "\\%", "#" => "\\#", "-" => "-{}" }
-- not \^{} because of https://github.com/Khan/KaTeX/issues/1366
-- -{} because of https://github.com/Khan/KaTeX/issues/1367
--texAltLiteral = s -> concatenate apply(characters s, c -> if texAltLiteralTable#?c then texAltLiteralTable#c else c)
texAltLiteral = s -> ( open:= {};
    concatenate apply(characters s,
    c -> first(if texAltLiteralTable#?c and #open === 0 then texAltLiteralTable#c else c,
	if #open > 0 then (
	    if (last open === mathJaxHtmlTag or last open === mathJaxOutputTag or last open === mathJaxTextTag) and c === mathJaxEndTag then open = drop(open,-1)
	    else if (last open === mathJaxInputTag or last open === mathJaxInputContdTag) and c === "\n" then open = drop(open,-1);
	),
	if c === mathJaxHtmlTag or c === mathJaxOutputTag or c === mathJaxInputTag or c === mathJaxInputContdTag or c === mathJaxTextTag then open = append(open,c)
	)
    )
)

htmlAltLiteralTable := hashTable { "&" => "&amp;", "<" => "&lt;", "]]>" => "]]&gt;", "\42" => "&quot;", "\\" => "&bsol;" }
--htmlAltLiteral = s -> concatenate apply(characters s, c -> if htmlAltLiteralTable#?c then htmlAltLiteralTable#c else c)
htmlAltLiteral = s -> ( open:= {};
    concatenate apply(characters s,
    c -> first(if htmlAltLiteralTable#?c and #open === 0 then htmlAltLiteralTable#c else c,
	if #open > 0 then (
	    if (last open === mathJaxHtmlTag or last open === mathJaxOutputTag or last open === mathJaxTextTag) and c === mathJaxEndTag then open = drop(open,-1)
	    else if (last open === mathJaxInputTag or last open === mathJaxInputContdTag) and c === "\n" then open = drop(open,-1);
	),
	if c === mathJaxHtmlTag or c === mathJaxOutputTag or c === mathJaxInputTag or c === mathJaxInputContdTag or c === mathJaxTextTag then open = append(open,c)
	)
    )
)


texMath String := s -> "\\texttt{" | texAltLiteral s | "%\n}" -- here we refuse to consider \n issues. the final %\n is for closing of inputTag if needed!

--texWrap := x -> concatenate("\\(",htmlLiteral x,"\\)") -- for mathJax compatibility
texWrap := x -> concatenate("\\(",x,"\\)") -- breaks mathJax compatibility (KaTeX mode!) but helps with other situations

mathJax Thing := x -> texWrap("\\displaystyle " | texMath x) -- by default, for MathJax we use tex (as opposed to html)

-- text stuff: we use html instead of tex, much faster (and better spacing)
mathJax Hypertext := html -- !
-- the % is relative to line-height
mathJax Net := n -> concatenate("<pre><span style=\"display:inline-table;vertical-align:", toString(100*(height n-1)), "%\">", apply(unstack n, x-> htmlLiteral x | "<br/>"), "</span></pre>")
mathJax String := x -> concatenate("<pre>", htmlAltLiteral x, "</pre>") -- only problem is, this ignores starting/ending \n. but then one should use Net for that
mathJax Descent := x -> concatenate("<span style=\"display:inline-table\"><pre>", sort apply(pairs x,
     (k,v) -> (
	  if #v === 0
	  then toString k -- sucks but no choice
	  else toString k | " : " | mathJax v
	  ) | "<br/>"), "</pre></span>")
-- some expressions can be mathJaxed directly w/o reference to texMath
mathJax Holder := x -> mathJax x#0
mathJax Describe := x -> mathJax x#0
-- kind of an expression analogue of Net
mathJax ColumnExpression := x -> concatenate("<span style=\"display:inline-flex;flex-direction:column\">", apply(toList x, mathJax), "</span>")
--mathJax RowExpression := x -> concatenate("<span style=\"display:inline-flex;flex-direction:row\">", apply(toList x, mathJax), "</span>")
mathJax RowExpression := x -> concatenate("<span>",apply(toList x, mathJax),"</span>")

-*
-- temporary HACK: a new Type should be created for examples since they won't literally be PRE in mathJax mode
-- either that or must rewrite the whole structure of mathJax = html, or both
*-

html PRE := x -> concatenate(
     "<pre>",
     if topLevelMode === MathJax then (mathJaxTextTag, x, "\n", mathJaxEndTag) else demark(newline, apply(lines concatenate x, htmlLiteral)), -- note the extra \n to make sure input is ended
     "</pre>\n"
     )

-- output routines

ZZ#{MathJax,InputPrompt} = lineno -> ZZ#{Standard,InputPrompt} lineno | mathJaxInputTag
ZZ#{MathJax,InputContinuationPrompt} = lineno -> mathJaxInputContdTag

Thing#{MathJax,BeforePrint} = identity -- not sure what to put there

Nothing#{MathJax,Print} = identity

Thing#{MathJax,Print} = x -> (
    oprompt := concatenate(interpreterDepth:"o", toString lineNumber, " = ");
    mathJaxBegin();
    y := mathJax x; -- we compute the mathJax now (in case it produces an error)
    mathJaxEnd();
    << endl << oprompt | mathJaxOutputTag | y | mathJaxEndTag << endl;
    )

-- afterprint <sigh>

on := () -> concatenate(interpreterDepth:"o", toString lineNumber)

texAfterPrint :=  y -> (
    mathJaxBegin();
    z := texMath if instance(y,Sequence) then RowExpression deepSplice y else y;
    mathJaxEnd();
    << endl << on() | " : " | mathJaxHtmlTag | texWrap z | mathJaxEndTag << endl;
    )

Thing#{MathJax,AfterPrint} = x -> texAfterPrint class x;

Boolean#{MathJax,AfterPrint} = identity

Expression#{MathJax,AfterPrint} = x -> texAfterPrint (Expression," of class ",class x)

Describe#{MathJax,AfterPrint} = identity

Ideal#{MathJax,AfterPrint} = Ideal#{MathJax,AfterNoPrint} = (I) -> texAfterPrint (Ideal," of ",ring I)
MonomialIdeal#{MathJax,AfterPrint} = MonomialIdeal#{MathJax,AfterNoPrint} = (I) -> texAfterPrint (MonomialIdeal," of ",ring I)

Module#{MathJax,AfterPrint} = M -> (
     n := rank ambient M;
     texAfterPrint(ring M,"-module",
     if M.?generators then
     if M.?relations then (", subquotient of ",ambient M)
     else (", submodule of ",ambient M)
     else if M.?relations then (", quotient of ",ambient M) 
     else if n > 0 then
	  (", free",
	  if not all(degrees M, d -> all(d, zero)) 
	  then (", degrees ",runLengthEncode if degreeLength M === 1 then flatten degrees M else apply(degrees M,runLengthEncode))
	  ))
     )


Matrix#{MathJax,AfterPrint} = Matrix#{MathJax,AfterNoPrint} = f -> texAfterPrint (Matrix, if isFreeModule target f and isFreeModule source f then (" ", new MapArrow from {target f,source f}))

Net#{MathJax,AfterPrint} = identity

Nothing#{MathJax,AfterPrint} = identity

RingMap#{MathJax,AfterPrint} = RingMap#{MathJax,AfterNoPrint} = f -> texAfterPrint (class f," ",new MapArrow from {target f,source f})

Sequence#{MathJax,AfterPrint} = Sequence#{MathJax,AfterNoPrint} = identity

CoherentSheaf#{MathJax,AfterPrint} = F -> (
     X := variety F;
     M := module F;
     n := rank ambient F;
     texAfterPrint("coherent sheaf on ",X,
     if M.?generators then
     if M.?relations then (", subquotient of ", ambient F)
     else (", subsheaf of ", ambient F)
     else if M.?relations then (", quotient of ", ambient F)
     else if n > 0 then (
	  ", free"
	  -- if not all(degrees M, d -> all(d, zero))
	  -- then << ", degrees " << if degreeLength M === 1 then flatten degrees M else degrees M;
	  )
     )
 )

ZZ#{MathJax,AfterPrint} = identity

-- experimental
print = x -> if topLevelMode === MathJax then (
    y := mathJax x; -- we compute the mathJax now (in case it produces an error)
    << mathJaxHtmlTag | y | mathJaxEndTag << endl;
    ) else ( << net x << endl; )

-- bb letters
ℚ=QQ
ℝ=RR
ℤ=ZZ
ℂ=CC

-- color
ColoredExpression = new HeaderType of Expression
net ColoredExpression := x -> net x#0
toString ColoredExpression := x -> toString x#0
texMath ColoredExpression := x -> "{\\color{" | x#1 | "}" | texMath x#0 | "}"
-- one could make that a method to have more specific coloring rules for certain types. anyway, not used for now
--coloredExpression = x -> (c:=color x; if c=!= null then ColoredExpression { expression x, c } else expression x)

color = method(Dispatch => Thing, TypicalValue => String)
color Keyword := x -> "#a020f0"
color Type := x -> "#228b22"
color Function := x -> "#0000ff"
color Constant := color Boolean := color ScriptedFunctor := x -> "#008b8b"
color Thing := x -> null
color Ring := color InexactFieldFamily := x -> "black" -- disagrees with the syntax highlighting; but must be so because expressions of rings are symbols anyway, so color will get lost

texMathDebug=false;
texMathBackup := texMath
-- the debug hack
texMathDebugWrapper := x -> (
    global texMath <- texMathBackup;
    y := texMathBackup class x;
    global texMath <- texMathDebugWrapper;
    "\\underset{\\tiny " | y | "}{\\boxed{" | texMathBackup x | "}}"
    )
-- the color hack
texMathColorWrapper := x -> (
    c:=color x;
    if c =!= null then "{\\color{" | c | "}" | texMathBackup x | "}" else texMathBackup x
    )
mathJaxBegin = () -> (
    if texMathDebug then
    global texMath <- texMathDebugWrapper
    else
    global texMath <- texMathColorWrapper
    )
mathJaxEnd = () -> (
    global texMath <- texMathBackup;
    )

-- completely unrelated -- move somewhere else
-- in any case, will fail because most net operations are at d level
width String := x -> ( -- we leave length to be #
    c := 0;
    scan(ascii x, i -> if (i & 192) =!= 128 then c=c+1);
    c
    )
width Net := x -> if #x === 0 then 0 else max apply(unstack x,width) -- kind of a lame hack, short circuits the internal width
