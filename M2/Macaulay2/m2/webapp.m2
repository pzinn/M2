-- Paul Zinn-Justin 2018-2020

-- topLevelMode=WebApp definitions
-- tags are required to help the browser app distinguish html from text
webAppTags := apply((17,18,19,20,28,29,30,(17,36),(36,18)),ascii);
    (	webAppHtmlTag,        -- indicates what follows is HTML ~ <span class='M2Html'>
	webAppEndTag,         -- closing tag ~ </span>
	webAppCellTag,        -- start of cell (bundled input + output) ~ <p>
	webAppCellEndTag,     -- closing tag for cell ~ </p>
	webAppInputTag,       -- it's text but it's input ~ <span class='M2Input'>
	webAppInputContdTag,  -- text, continuation of input
	webAppUrlTag,         -- used internally to follow URLs
	webAppTexTag,         -- effectively deprecated, ~ <span class='M2Html'> $
	webAppTexEndTag       -- effectively deprecated, ~ $ </span>
	)=webAppTags;

webAppTagsRegex := concatenate("[",drop(webAppTags,-2),"]")

htmlInner = (x,mode) -> ( -- current mode = false: html, true: tex
    newmode := lookup(html,class x) === lookup(html,Thing) or instance(x,Expression);
    y := if newmode then texMath'(texMathInside,x) else html x;
    -- no recursing yet for html
    if debugLevel === 42 then (
	y = concatenate(
	    "\\underset{\\tiny ",
	     texMath class x,
	     "}{\\fcolorbox{gray}{transparent}{$",
	     if not newmode then webAppHtmlTag,
	     y,
	     if not newmode then webAppEndTag,
	     "$}}"
	     );
	newmode=true;
	);
    concatenate(
    if mode =!= newmode then if newmode then "\\(" else webAppHtmlTag,
    y,
    if mode =!= newmode then if newmode then "\\)" else webAppEndTag
    ))

texMathInside = x -> htmlInner(x,true);
htmlInside = x -> htmlInner(x,false); -- only used at top level -- no recursing inside html for now

-- output routines for WebApp mode

ZZ#{WebApp,InputPrompt} = lineno -> concatenate(
    webAppCellEndTag, -- close previous cell
    webAppCellTag,
    interpreterDepth:"i",
    toString lineno,
    " : ",
    webAppInputTag)

ZZ#{WebApp,InputContinuationPrompt} = lineno -> webAppInputContdTag

Thing#{WebApp,BeforePrint} = identity

Nothing#{WebApp,Print} = identity

Thing#{WebApp,Print} = x -> (
    oprompt := concatenate(interpreterDepth:"o", toString lineNumber, " = ");
    y := htmlInside x; -- we compute the html now (in case it produces an error)
    if class y =!= String then error "invalid html output";
    << endl << oprompt | webAppHtmlTag | y | webAppEndTag << endl;
    )

InexactNumber#{WebApp,Print} = x ->  withFullPrecision ( () -> Thing#{WebApp,Print} x )

-- afterprint

on := () -> concatenate(interpreterDepth:"o", toString lineNumber)

htmlAfterPrint :=  x -> (
    if class x === Sequence then x = RowExpression deepSplice { x };
    y := htmlInside x; -- we compute the html now (in case it produces an error)
    if class y =!= String then error "invalid html output";
    << endl << on() | " : " | webAppHtmlTag | y | webAppEndTag << endl;
    )

Thing#{WebApp,AfterPrint} = x -> htmlAfterPrint class x;

Boolean#{WebApp,AfterPrint} = identity

Expression#{WebApp,AfterPrint} = x -> htmlAfterPrint (Expression," of class ",class x)

Describe#{WebApp,AfterPrint} = identity

Ideal#{WebApp,AfterPrint} = Ideal#{WebApp,AfterNoPrint} = (I) -> htmlAfterPrint (Ideal," of ",ring I)
MonomialIdeal#{WebApp,AfterPrint} = MonomialIdeal#{WebApp,AfterNoPrint} = (I) -> htmlAfterPrint (MonomialIdeal," of ",ring I)

InexactNumber#{WebApp,AfterPrint} = x -> htmlAfterPrint (class x," (of precision ",precision x,")")

Module#{WebApp,AfterPrint} = M -> htmlAfterPrint(
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

Net#{WebApp,AfterPrint} = identity

Nothing#{WebApp,AfterPrint} = identity

Matrix#{WebApp,AfterPrint} = Matrix#{WebApp,AfterNoPrint} =
RingMap#{WebApp,AfterPrint} = RingMap#{WebApp,AfterNoPrint} = f -> htmlAfterPrint (class f, " ", new MapExpression from {target f,source f})

-- Sequence#{WebApp,AfterPrint} = Sequence#{WebApp,AfterNoPrint} = identity

CoherentSheaf#{WebApp,AfterPrint} = F -> (
     X := variety F;
     M := module F;
     n := rank ambient F;
     htmlAfterPrint("coherent sheaf on ",X,
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

if topLevelMode === WebApp then (
    compactMatrixForm = false;
    -- the help hack: if started in WebApp mode, help is compiled in it as well
    processExamplesLoop ExampleItem := (x->new LITERAL from replace("\\$\\{prefix\\}","usr",x#0)) @@ (lookup(processExamplesLoop,ExampleItem));
    -- the help hack 2 (incidentally, this regex is safer than in standard mode)
    M2outputRE      = "(?="|webAppCellTag|")";
    -- the print hack
    print = x -> if topLevelMode === WebApp then (
	y := htmlInside x; -- we compute the html now (in case it produces an error)
	if class y =!= String then error "invalid html output";
	<< webAppHtmlTag | y | webAppEndTag << endl;
	) else ( << net x << endl; );
    -- the show hack
    showURL := lookup(show,URL);
    show URL := url -> if topLevelMode === WebApp then (<< webAppUrlTag | url#0 | webAppEndTag;) else showURL url;
    -- the error hack
    oldolderror := olderror;
    olderror = args -> oldolderror apply(deepSplice sequence args, s -> replace(webAppTagsRegex," ",s));
    -- the userSymbols hack (TEMP): by now mostly differs in "robust" stuff
    listSymbols List := x -> Describe TABLE prepend(
     apply({"symbol", "class", "value", "location of symbol"},s->TH {s}),
     apply(x, y -> apply({y,short class value y,short value y,TT symbolLocation y},s->TD {s}))
     );
    -- redefine htmlLiteral to exclude codes
    htmlLiteral0 := htmlLiteral;
    htmlLiteral = (s -> if s === null then null else replace(webAppTagsRegex," ",s)) @@ htmlLiteral0;
    -- but should affect html Thing differently:
    htmlLiteral1 := s -> if s === null then s else (
	depth := -1;
	concatenate apply(separate("(?="|webAppTagsRegex|")",s), x -> (
		if #x>0 and x#0 === webAppEndTag then depth=depth-1 else depth=depth+1;
		if depth <= 0 then htmlLiteral0 x else x
		)));
    html Monoid :=
    html RingFamily :=
    html Ring :=
    html Thing := htmlLiteral1 @@ tex; -- ugly
)
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
htmlBackup := html;
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
    if lookup(html,class x) === lookup(html,Thing) then concatenate(
	c := try colorTable#x else color x;
	if c =!= null then "\\begingroup\\color{" | c | "}",
	if lookup(texMath,class x) === Thing#texMath then texMath'(texMathInsideColor,x) else texMath x,
	if c =!= null then "\\endgroup "
	) else concatenate(
	webAppHtmlTag,
	html x,
	webAppEndTag
	))


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

