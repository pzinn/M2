-- Paul Zinn-Justin 2018

-- htmlWithTex Thing produces some valid html code with possible TeX code in \( \)
-- topLevelMode=WebApp produces that plus possible pure text coming from the system
-- hence, requires tags to help the browser app distinguish html from text
(webAppEndTag,            -- closing tag
    webAppHtmlTag,        -- indicates what follows is HTML
    webAppOutputTag,      -- it's html but it's output
    webAppInputTag,       -- it's text but it's input
    webAppInputContdTag,  -- text, continuation of input
    webAppTextTag):=      -- other text
apply((17,18,19,20,28,30),ascii)
-- what follows probably needs simplifying -- we're trying not to code stuff inside web app tags
texAltLiteral = s -> ( open:= {};
    concatenate apply(characters s,
    c -> first(if texAltLiteralTable#?c and #open === 0 then texAltLiteralTable#c else c,
	if #open > 0 then (
	    if (last open === webAppHtmlTag or last open === webAppOutputTag or last open === webAppTextTag) and c === webAppEndTag then open = drop(open,-1)
	    else if (last open === webAppInputTag or last open === webAppInputContdTag) and c === "\n" then open = drop(open,-1);
	),
	if c === webAppHtmlTag or c === webAppOutputTag or c === webAppInputTag or c === webAppInputContdTag or c === webAppTextTag then open = append(open,c)
	)
    )
)

htmlWithTexLiteral = s -> replace("\\\\","&bsol;",htmlLiteral s);

--texWrap := x -> concatenate("\\(",htmlLiteral x,"\\)") -- for MathJax compatibility
texWrap := x -> concatenate("\\(",x,"\\)")

htmlWithTex Thing := x -> texWrap("\\displaystyle " | texMath x) -- by default, for KaTeX we use tex (as opposed to html)

-- text stuff: we use html instead of tex, much faster (and better spacing)
htmlWithTex Hypertext := x -> replace("\\\\","&bsol;",html x);
-- the following lines could in principle be for html itself rather than htmlWithTex (and then use the line above for htmlWithTex);
-- but they conflict with the current defs
-- the % is relative to line-height
htmlWithTex Net := n -> concatenate("<pre><span style=\"display:inline-table;vertical-align:",
    toString(100*(height n-1)), "%\">", apply(unstack n, x-> htmlWithTexLiteral x | "<br/>"), "</span></pre>")
htmlWithTex String := x -> concatenate("<pre>", htmlWithTexLiteral x, "</pre>") -- only problem is, this ignores starting/ending \n. but then one should use Net for that
htmlWithTex Descent := x -> concatenate("<span style=\"display:inline-table\"><pre>", sort apply(pairs x,
     (k,v) -> (
	  if #v === 0
	  then toString k -- sucks but no choice
	  else toString k | " : " | htmlWithTex v
	  ) | "<br/>"), "</pre></span>")
-- some expressions can be htmlWithTex'ed directly w/o reference to texMath
htmlWithTex Holder := x -> htmlWithTex x#0
-- kind of an expression analogue of Net
htmlWithTex ColumnExpression := x -> concatenate("<span style=\"display:inline-flex;flex-direction:column\">", apply(toList x, htmlWithTex), "</span>")
--htmlWithTex RowExpression := x -> concatenate("<span style=\"display:inline-flex;flex-direction:row\">", apply(toList x, htmlWithTex), "</span>")
htmlWithTex RowExpression := x -> concatenate("<span>",apply(toList x, htmlWithTex),"</span>")

-*
-- temporary HACK: a new Type should be created for examples since they won't literally be PRE in htmlWithTex mode
-- either that or must rewrite the whole structure of htmlWithTex = html, or both
*-

html PRE := x -> concatenate(
     "<pre>",
     if topLevelMode === WebApp then (webAppTextTag, x, "\n", webAppEndTag) else demark(newline, apply(lines concatenate x, htmlLiteral)), -- note the extra \n to make sure input is ended
     "</pre>\n"
     )

-- output routines

ZZ#{WebApp,InputPrompt} = lineno -> ZZ#{Standard,InputPrompt} lineno | webAppInputTag
ZZ#{WebApp,InputContinuationPrompt} = lineno -> webAppInputContdTag

Thing#{WebApp,BeforePrint} = identity -- not sure what to put there

Nothing#{WebApp,Print} = identity

Thing#{WebApp,Print} = x -> (
    oprompt := concatenate(interpreterDepth:"o", toString lineNumber, " = ");
    webAppBegin();
    y := htmlWithTex x; -- we compute the htmlWithTex now (in case it produces an error)
    webAppEnd();
    << endl << oprompt | webAppOutputTag | y | webAppEndTag << endl;
    )

InexactNumber#{WebApp,Print} = x ->  withFullPrecision ( () -> Thing#{WebApp,Print} x )

-- afterprint <sigh>

on := () -> concatenate(interpreterDepth:"o", toString lineNumber)

texAfterPrint :=  y -> (
    webAppBegin();
    z := texMath if instance(y,Sequence) then RowExpression deepSplice y else y;
    webAppEnd();
    << endl << on() | " : " | webAppHtmlTag | texWrap z | webAppEndTag << endl;
    )

Thing#{WebApp,AfterPrint} = x -> texAfterPrint class x;

Boolean#{WebApp,AfterPrint} = identity

Expression#{WebApp,AfterPrint} = x -> texAfterPrint (Expression," of class ",class x)

Describe#{WebApp,AfterPrint} = identity

Ideal#{WebApp,AfterPrint} = Ideal#{WebApp,AfterNoPrint} = (I) -> texAfterPrint (Ideal," of ",ring I)
MonomialIdeal#{WebApp,AfterPrint} = MonomialIdeal#{WebApp,AfterNoPrint} = (I) -> texAfterPrint (MonomialIdeal," of ",ring I)

InexactNumber#{WebApp,AfterPrint} = x -> texAfterPrint (class x," (of precision ",precision x,")")

Module#{WebApp,AfterPrint} = M -> (
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


Matrix#{WebApp,AfterPrint} = Matrix#{WebApp,AfterNoPrint} = f -> texAfterPrint (Matrix, if isFreeModule target f and isFreeModule source f then (" ", new MapExpression from {target f,source f}))

Net#{WebApp,AfterPrint} = identity

Nothing#{WebApp,AfterPrint} = identity

RingMap#{WebApp,AfterPrint} = RingMap#{WebApp,AfterNoPrint} = f -> texAfterPrint (class f," ",new MapExpression from {target f,source f})

Sequence#{WebApp,AfterPrint} = Sequence#{WebApp,AfterNoPrint} = identity

CoherentSheaf#{WebApp,AfterPrint} = F -> (
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

ZZ#{WebApp,AfterPrint} = identity

-- experimental
print = x -> if topLevelMode === WebApp then (
    y := htmlWithTex x; -- we compute the htmlWithTex now (in case it produces an error)
    << webAppHtmlTag | y | webAppEndTag << endl;
    ) else ( << net x << endl; )

-- bb letters
export { "ℚ","ℝ","ℤ","ℂ","ℙ","∞" }
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

toExtString := method() -- somewhere between toString and toExternalString <sigh>
toExtString Thing := toString -- e.g. for a ring!
toExtString Symbol := toExternalString -- e.g. for a ring element!
toExtString String := toExternalString

texMathDebug=false;
texMathBackup := texMath
-- the debug hack
texMathDebugWrapper := x -> (
-*    global texMath <- texMathBackup;
    y := texMathBackup class x;
    global texMath <- texMathDebugWrapper;
    "\\underset{\\tiny " | y | "}{\\boxed{" | texMathBackup x | "}}" *-
    if instance(x,VisibleList) or instance(x,Expression)
    then "\\rawhtml{<span class='M2Meta' data-type='"|toString class x|"'>}{0em}{0em}"|texMathBackup x|"\\rawhtml{</span>}{0em}{0em}" else (
	y := expression x;
	if instance(y,Holder)
	then "\\rawhtml{<span class='M2Meta' data-content='"|toExtString x|"'>}{0em}{0em}"|texMathBackup x|"\\rawhtml{</span>}{0em}{0em}" else texMath y
    )
)
-- the color hack
texMathColorWrapper := x -> (
    c := try colorTable#x else color x;
    if c =!= null then "\\begingroup\\color{" | c | "}" | texMathBackup x | "\\endgroup " else texMathBackup x
    -- buggy, see https://github.com/Khan/KaTeX/issues/1679
    )
webAppBegin = () -> (
    if texMathDebug then
    global texMath <- texMathDebugWrapper
    else
    global texMath <- texMathColorWrapper
    )
webAppEnd = () -> (
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
