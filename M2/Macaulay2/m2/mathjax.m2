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
-- what follows probably needs simplifying -- we're trying not to code stuff inside mathjax tags
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

htmlAltLiteralTable = hashTable { "&" => "&amp;", "<" => "&lt;", "]]>" => "]]&gt;", "\42" => "&quot;", "\\" => "&bsol;" }
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


Matrix#{MathJax,AfterPrint} = Matrix#{MathJax,AfterNoPrint} = f -> texAfterPrint (Matrix, if isFreeModule target f and isFreeModule source f then (" ", new MapExpression from {target f,source f}))

Net#{MathJax,AfterPrint} = identity

Nothing#{MathJax,AfterPrint} = identity

RingMap#{MathJax,AfterPrint} = RingMap#{MathJax,AfterNoPrint} = f -> texAfterPrint (class f," ",new MapExpression from {target f,source f})

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
∞=infinity

-- color
ColoredExpression = new HeaderType of Expression
net ColoredExpression := x -> net x#0
toString ColoredExpression := x -> toString x#0
texMath ColoredExpression := x -> "\\begingroup\\color{" | x#1 | "}" | texMath x#0 | "\\endgroup "
-- one could make that a method to have more specific coloring rules for certain types. anyway, not used for now
--coloredExpression = x -> (c:=color x; if c=!= null then ColoredExpression { expression x, c } else expression x)

color = method(Dispatch => Thing, TypicalValue => String)
color Keyword := x -> "#a020f0"
color Type := x -> "#228b22"
color Function := x -> "#0000ff"
color Constant := color Boolean := color ScriptedFunctor := x -> "#008b8b"
color Thing := x -> null
color Ring := color InexactFieldFamily := x -> "black" -- disagrees with the syntax highlighting; but must be so because expressions of rings are symbols anyway, so color will get lost
-- or can use the colorTable for that. but do I really want rings to be colored? don't think so.
-- or should be another color altogether
colorTable = new MutableHashTable
--setColor = (x,c) -> (colorTable#x = colorTable#(unhold expression x) = toString c;) -- slightly overkill
setColor = (x,c) -> (colorTable#(unhold expression x) = toString c;) -- should be all we need

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
    c := try colorTable#x else color x;
    if c =!= null then "\\begingroup\\color{" | c | "}" | texMathBackup x | "\\endgroup " else texMathBackup x
    -- buggy, see https://github.com/Khan/KaTeX/issues/1679
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
