htmlAltLiteralTable := hashTable { "&" => "&amp;", "<" => "&lt;", "]]>" => "]]&gt;", "\42" => "&quot;", "\\" => "&bsol;" }
htmlAltLiteral = s -> concatenate apply(characters s, c -> if htmlAltLiteralTable#?c then htmlAltLiteralTable#c else c)

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

--texWrap := x -> concatenate("\\(",htmlLiteral x,"\\)") -- for mathJax compatibility
texWrap := x -> concatenate("\\(",x,"\\)") -- breaks mathJax compatibility (KaTeX mode!) but helps with other situations

mathJax Thing := x -> texWrap("\\displaystyle " | texMath x) -- by default, for KaTeX we use tex (as opposed to html)

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
mathJax RowExpression := x -> concatenate("<span>",apply(toList x, mathJax),"</span>")
mathJax Holder := x -> mathJax x#0
mathJax Describe := x -> mathJax x#0

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
	  then (", degrees ",runLengthEncode if degreeLength M === 1 then flatten degrees M else degrees M)
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

-- bb letters
export { "ℚ","ℝ","ℤ","ℂ","∞" }
ℚ=QQ
ℝ=RR
ℤ=ZZ
ℂ=CC
∞=infinity

-- the debug hack (temporary, to be removed before PR)
texMathDebug=false;
texMathBackup := texMath
texMathDebugWrapper := x -> (
    global texMath <- texMathBackup;
    y := texMathBackup class x;
    global texMath <- texMathDebugWrapper;
    "\\underset{\\tiny " | y | "}{\\boxed{" | texMathBackup x | "}}"
    )
mathJaxBegin = () -> (
    if texMathDebug then
    global texMath <- texMathDebugWrapper
    )
mathJaxEnd = () -> (
    if texMathDebug then
    global texMath <- texMathBackup;
    )
