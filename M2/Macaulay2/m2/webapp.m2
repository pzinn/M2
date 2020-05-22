-- Paul Zinn-Justin 2018

-- htmlWithTex Thing produces some valid html code with possible TeX code
-- topLevelMode=WebApp produces that plus possible pure text coming from the system
-- hence, requires tags to help the browser app distinguish html from text
(webAppEndTag,            -- closing tag
    webAppHtmlTag,        -- indicates what follows is HTML
    webAppOutputTag,      -- it's html but it's output
    webAppInputTag,       -- it's text but it's input
    webAppInputContdTag,  -- text, continuation of input
    webAppTextTag,        -- other text
    webAppTexTag,         -- TeX start
    webAppTexEndTag       -- TeX end
    ):=("</span>","<span class='M2Html'>","<span class='M2Html M2Output'>","","","<span class='M2Text'>","\\(","\\)");

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

htmlWithTex Thing := tex -- by default, we use tex (as opposed to html)

-- text stuff: we use html instead of tex, much faster (and better spacing)
htmlWithTex Hypertext := html
-- the following lines could in principle be for html itself rather than htmlWithTex (and then use the line above for htmlWithTex);
-- but they conflict with the current defs
htmlWithTex Net := n -> concatenate("<pre style=\"display:inline-table;vertical-align:",
    toString(100*(height n-1)), "%\">\n", apply(unstack n, x-> htmlLiteral x | "<br/>"), "</pre>") -- the % is relative to line-height
htmlWithTex String := x -> concatenate("<pre style=\"display:inline\">\n", htmlLiteral x, "</pre>",
    if #x>0 and last x === "\n" then "<br/>") -- fix for html ignoring trailing \n
htmlWithTex Descent := x -> concatenate("<pre style=\"display:inline-table\">\n", sort apply(pairs x,
     (k,v) -> (
	  if #v === 0
	  then htmlWithTex net k -- sucks but no choice
	  else htmlWithTex net k | " : " | htmlWithTex v
	  ) | "<br/>"), "</pre>")

-- now preparation for output

texMathStartBackup := texMathEndBackup := "$"; -- the default tex delimiters

webAppBegin = (displayStyle) -> (
    texMathStartBackup = texMathStart;
    texMathEndBackup = texMathEnd;
    texMathStart = webAppTexTag | (if displayStyle then "\\displaystyle " else "");
    texMathEnd = webAppTexEndTag;
    );
webAppEnd = () -> (
    texMathStart = texMathStartBackup;
    texMathEnd = texMathEndBackup;
    );

-- now preparation for output

-- both of the functions below are activated with texMath <- texMath[Color]Wrapper
-- the debug hack -- the rawhtml is TEMP, of course. currently deactivated
toExtString := method() -- somewhere between toString and toExternalString <sigh>
toExtString Thing := toString -- e.g. for a ring!
toExtString Symbol := toExternalString -- for indexedvariables, for ex...
toExtString String := toExternalString
texMathWrapper = x -> (
    if instance(x,VisibleList) or instance(x,Expression)
    then "\\rawhtml{<span class='M2Meta' data-type='"|toString class x|"'>}{0em}{0em}"|texMathBackup x|"\\rawhtml{</span>}{0em}{0em}"
    else (
	e := expression x;
	if instance(e,Holder) and e#0 === x then (
	global texMath <- texMathBackup;
	first("\\rawhtml{<span class='M2Meta' data-content='"|toExtString x|"'>}{0em}{0em}"|texMath x|"\\rawhtml{</span>}{0em}{0em}",
	    global texMath <- texMathWrapper)
	)
    else texMathBackup x
    )
)
-- the color hack: currently deactivated
texMathColorWrapper := x -> (
    c := try colorTable#x else color x;
    if c =!= null then "\\begingroup\\color{" | c | "}" | texMathBackup x | "\\endgroup " else texMathBackup x
    -- hopefully no longer buggy, see https://github.com/Khan/KaTeX/issues/1679
    )
--

-- output routines

ZZ#{WebApp,InputPrompt} = lineno -> ZZ#{Standard,InputPrompt} lineno | webAppInputTag
ZZ#{WebApp,InputContinuationPrompt} = lineno -> webAppInputContdTag

Thing#{WebApp,BeforePrint} = identity -- not sure what to put there

Nothing#{WebApp,Print} = identity

Thing#{WebApp,Print} = x -> (
    oprompt := concatenate(interpreterDepth:"o", toString lineNumber, " = ");
    webAppBegin(true);
    y := htmlWithTex x; -- we compute the htmlWithTex now (in case it produces an error)
    webAppEnd();
    << endl << oprompt | webAppOutputTag | y | webAppEndTag << endl;
    )

InexactNumber#{WebApp,Print} = x ->  withFullPrecision ( () -> Thing#{WebApp,Print} x )

-- afterprint

on := () -> concatenate(interpreterDepth:"o", toString lineNumber)

htmlWithTexAfterPrint :=  y -> (
    y=deepSplice sequence y;
    webAppBegin(false);
    z := htmlWithTex \ y;
    webAppEnd();
    << endl << on() | " : " | webAppHtmlTag | concatenate z | webAppEndTag << endl;
    )

Thing#{WebApp,AfterPrint} = x -> htmlWithTexAfterPrint class x;

Boolean#{WebApp,AfterPrint} = identity

Expression#{WebApp,AfterPrint} = x -> htmlWithTexAfterPrint (Expression," of class ",class x)

Describe#{WebApp,AfterPrint} = identity

Ideal#{WebApp,AfterPrint} = Ideal#{WebApp,AfterNoPrint} = (I) -> htmlWithTexAfterPrint (Ideal," of ",ring I)
MonomialIdeal#{WebApp,AfterPrint} = MonomialIdeal#{WebApp,AfterNoPrint} = (I) -> htmlWithTexAfterPrint (MonomialIdeal," of ",ring I)

InexactNumber#{WebApp,AfterPrint} = x -> htmlWithTexAfterPrint (class x," (of precision ",precision x,")")

Module#{WebApp,AfterPrint} = M -> htmlWithTexAfterPrint(
    ring M,"-module",
    if M.?generators then
    if M.?relations then (", subquotient of ",ambient M)
    else (", submodule of ",ambient M)
    else if M.?relations then (", quotient of ",ambient M)
    else if rank ambient M > 0 then
    (", free",
	if not all(degrees M, d -> all(d, zero))
	then (", degrees ",runLengthEncode if degreeLength M === 1 then flatten degrees M else degrees M)
	)
    )

Matrix#{WebApp,AfterPrint} = Matrix#{WebApp,AfterNoPrint} = f -> htmlWithTexAfterPrint (Matrix, if isFreeModule target f and isFreeModule source f then (" ", new MapExpression from {target f,source f}))

Net#{WebApp,AfterPrint} = identity

Nothing#{WebApp,AfterPrint} = identity

RingMap#{WebApp,AfterPrint} = RingMap#{WebApp,AfterNoPrint} = f -> htmlWithTexAfterPrint (class f," ",new MapExpression from {target f,source f})

Sequence#{WebApp,AfterPrint} = Sequence#{WebApp,AfterNoPrint} = identity

CoherentSheaf#{WebApp,AfterPrint} = F -> (
     X := variety F;
     M := module F;
     n := rank ambient F;
     htmlWithTexAfterPrint("coherent sheaf on ",X,
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
    webAppBegin(true);
    y := htmlWithTex x; -- we compute the htmlWithTex now (in case it produces an error)
    webAppEnd();
    << webAppHtmlTag | y | webAppEndTag << endl;
    ) else ( << net x << endl; )

-- bb letters (to be removed before PR)
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



-- completely unrelated -- move somewhere else. see also
-- https://github.com/Macaulay2/M2/issues/1069#issuecomment-617790397
-- in any case, will fail because most net operations are at d level
width String := x -> ( -- we leave length to be #
    c := 0;
    scan(ascii x, i -> if (i & 192) =!= 128 then c=c+1);
    c
    )
width Net := x -> if #x === 0 then 0 else max apply(unstack x,width) -- kind of a lame hack, short circuits the internal width

if topLevelMode === WebApp then (
    (webAppEndTag,            -- closing tag
	webAppHtmlTag,        -- indicates what follows is HTML
	webAppOutputTag,      -- it's html but it's output
	webAppInputTag,       -- it's text but it's input
	webAppInputContdTag,  -- text, continuation of input
	webAppTextTag,        -- other text
	webAppTexTag,         -- TeX start
	webAppTexEndTag       -- TeX end
	)=apply((17,18,19,20,28,30,31,17),ascii);
    -- the help hack: if started in WebApp mode, help is compiled in it as well
    webAppPRE := new MarkUpType of PRE;
    html webAppPRE := x -> concatenate( -- we really mean this: the browser will interpret it as pure text so no need to htmlLiteral it
	"<pre>",
	webAppTextTag, x, "\n", webAppEndTag,
	"</pre>\n"
	);
    pELBackup:=lookup(processExamplesLoop,ExampleItem);
    processExamplesLoop ExampleItem := x -> (
	res := pELBackup x;
	new webAppPRE from res );
    -- the print hack
    print = x -> if topLevelMode === WebApp then (
	webAppBegin(true);
	y := htmlWithTex x; -- we compute the htmlWithTex now (in case it produces an error)
	webAppEnd();
	<< webAppHtmlTag | y | webAppEndTag << endl;
	) else ( << net x << endl; );
    -- the texMath hack
    currentPackage#"exported mutable symbols"=append(currentPackage#"exported mutable symbols",global texMath);
    texMathBackup := texMath;
    texMathInsideHtml := x -> if lookup(htmlWithTex,class x) -* =!= html *- === tex then texMathBackup x else concatenate(
	webAppHtmlTag,
	htmlWithTex x,
	webAppEndTag
	);
    webAppBegin = (displayStyle) -> (
	texMathStartBackup = texMathStart;
	texMathEndBackup = texMathEnd;
	texMathStart = webAppTexTag | (if displayStyle then "\\displaystyle " else "");
	texMathEnd = webAppTexEndTag;
	global texMath <- texMathInsideHtml;
    );
    webAppEnd = () -> (
	texMathStart = texMathStartBackup;
	texMathEnd = texMathEndBackup;
	global texMath <- texMathBackup;
    );
)

