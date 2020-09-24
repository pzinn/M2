-- Paul Zinn-Justin 2018

-- htmlWithTex Thing produces some valid html code with possible TeX code
-- topLevelMode=WebApp produces that plus possible pure text coming from the system
-- hence, requires tags to help the browser app distinguish html from text
webAppTags := apply((17,18,19,20,28,29,30,31,17),ascii);
    (webAppEndTag,            -- closing tag ~ </span>
	webAppHtmlTag,        -- indicates what follows is HTML ~ <span class='M2Html'>
	webAppOutputTag,      -- it's html but it's output ~ <span class='M2Html M2Output'>
	webAppInputTag,       -- it's text but it's input ~ <span class='M2Input'>
	webAppInputContdTag,  -- text, continuation of input
	webAppUrlTag,         -- used internally
	webAppTextTag,        -- other text ~ <span class='M2Text'>
	webAppTexTag,         -- TeX start ~ \(
	webAppTexEndTag       -- TeX end ~ \)
	)=webAppTags;

htmlWithTexInner = (x,mode) -> ( -- current mode = false: html, true: tex
    newmode := lookup(htmlWithTex,class x) === tex;
    y := if newmode then if lookup(texMath,class x) === Thing#texMath then texMath'(texMathInside,x) else texMath x else htmlWithTex x;
    if debugLevel === 42 then (
	y = concatenate(
	    "\\underset{\\tiny ",
	     texMath class x,
	     "}{\\boxed{",
	     if not newmode then webAppHtmlTag,
	     y,
	     if not newmode then webAppEndTag,
	     "}}"
	     );
	newmode=true;
	);
    concatenate(
    if mode =!= newmode then if newmode then webAppTexTag|"\\displaystyle " else webAppHtmlTag,
    y,
    if mode =!= newmode then webAppEndTag
    ))

texMathInside = x -> htmlWithTexInner(x,true);
htmlInside = x -> htmlWithTexInner(x,false); -- only used at top level -- no recursing inside html for now

stripTags := s -> replace(concatenate("[",webAppTags,"]"),"",s)

htmlWithTex Thing := tex -- by default, we use tex (as opposed to html)
-- text stuff: we use html instead of tex, much faster (and better spacing)
htmlWithTex Hypertext := html
-- the following lines could in principle be for html itself rather than htmlWithTex (and then use the line above for htmlWithTex);
-- but they conflict with the current defs -- and would cause problems inside Hypertext
htmlWithTex Net := n -> concatenate("<pre style=\"display:inline-table;vertical-align:",
    toString(100*(height n-1)), "%\">\n", apply(unstack n, x-> stripTags htmlLiteral x | "<br/>"), "</pre>") -- the % is relative to line-height
htmlWithTex String := x -> concatenate("<pre style=\"display:inline\">\n", stripTags htmlLiteral x,
    if #x>0 and last x === "\n" then " ", -- fix for html ignoring trailing \n
    "</pre>")
htmlWithTex Descent := x -> concatenate("<pre style=\"display:inline-table\">\n", sort apply(pairs x,
     (k,v) -> (
	  if #v === 0
	  then htmlWithTex net k -- sucks but no choice
	  else htmlWithTex net k | " : " | htmlWithTex v
	  ) | "<br/>"), "</pre>")


-- obsolete -- function below used to be activated with texMath <- texMath[Color]Wrapper
-- the debug hack -- the rawhtml is TEMP, of course. currently deactivated
-*
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
-- the debug hack (temporary, to be removed before PR -- don't forget to remove the corresponding stuff in webAppBegin/End)
expressionDebug=false;
texMathBackup := texMath
htmlWithTexBackup := htmlWithTex;
 expressionDebugWrapper := x -> (
    if instance(x,VisibleList) or instance(x,Expression) then (
	global texMath <- texMathBackup;
	y := texMath class x;
	global texMath <- expressionDebugWrapper;
	z := texMathBackup x;
	)
    else (
	e := expression x;
	if instance(e, Holder) and e#0 === x then (
	global texMath <- texMathBackup;
	y = texMath class x;
	z = texMath x;
	global texMath <- expressionDebugWrapper;
	)
    else return texMathBackup x;
    );
    "\\underset{\\tiny " | y | "}{\\boxed{" | z | "}}"
    )
*-

-- the color hack: currently deactivated
-- hopefully no longer buggy, see https://github.com/Khan/KaTeX/issues/1679

texMathInsideColor = x -> (
    if lookup(htmlWithTex,class x) === tex then concatenate(
	c := try colorTable#x else color x;
	if c =!= null then "\\begingroup\\color{" | c | "}",
	if lookup(texMath,class x) === Thing#texMath then texMath'(texMathInsideColor,x) else texMath x,
	if c =!= null then "\\endgroup "
	) else concatenate(
	webAppHtmlTag,
	htmlWithTex x,
	webAppEndTag
	))


-- output routines for WebApp mode

ZZ#{WebApp,InputPrompt} = lineno -> ZZ#{Standard,InputPrompt} lineno | webAppInputTag
ZZ#{WebApp,InputContinuationPrompt} = lineno -> webAppInputContdTag

Thing#{WebApp,BeforePrint} = identity -- not sure what to put there

Nothing#{WebApp,Print} = identity

Thing#{WebApp,Print} = x -> (
    oprompt := concatenate(interpreterDepth:"o", toString lineNumber, " = ");
    y := htmlInside x; -- we compute the htmlWithTex now (in case it produces an error)
    if class y =!= String then error "invalid TeX/HTML output";
    << endl << oprompt | webAppOutputTag | y | webAppEndTag << endl;
    )

InexactNumber#{WebApp,Print} = x ->  withFullPrecision ( () -> Thing#{WebApp,Print} x )

-- afterprint

on := () -> concatenate(interpreterDepth:"o", toString lineNumber)

htmlWithTexAfterPrint :=  y -> (
    y=deepSplice sequence y;
    z := htmlInside \ y;
    if any(z, x -> class x =!= String) then error "invalid TeX/HTML output";
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
	new webAppPRE from res#0 );
    -- the print hack
    print = x -> if topLevelMode === WebApp then (
	y := htmlInside x; -- we compute the htmlWithTex now (in case it produces an error)
	<< webAppHtmlTag | y | webAppEndTag << endl;
	) else ( << net x << endl; );
)
