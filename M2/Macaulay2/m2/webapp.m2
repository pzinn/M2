-- Paul Zinn-Justin 2018-2020

needs "expressions.m2"
needs "matrix1.m2"
needs "monideal.m2"
needs "varieties.m2"

-- topLevelMode=WebApp definitions
-- tags are required to help the browser app distinguish html from text
webAppTags := apply((17,18,19,20,28,29,30,14,21,(17,36),(36,18)),ascii);
    (	webAppHtmlTag,        -- indicates what follows is HTML ~ <span class='M2Html'>
	webAppEndTag,         -- closing tag ~ </span>
	webAppCellTag,        -- start of cell (bundled input + output) ~ <p>
	webAppCellEndTag,     -- closing tag for cell ~ </p>
	webAppInputTag,       -- it's text but it's input ~ <span class='M2Input'>
	webAppInputContdTag,  -- text, continuation of input
	webAppUrlTag,         -- used internally to follow URLs
	webAppPromptTag,      -- input/output prompt
	webAppPositionTag,    -- code position (row:col)
	webAppTexTag,         -- effectively deprecated, ~ <span class='M2Html'> $
	webAppTexEndTag       -- effectively deprecated, ~ $ </span>
	)=webAppTags;

webAppTagsRegex := concatenate("[",drop(webAppTags,-2),"]")

-- output routines for WebApp mode

recordPosition = () -> if currentFileName == "stdio" then ( -- for now only stdio recorded
    webAppPositionTag,
--    toString currentFileName,
--    ":",
    toString currentLineNumber(), -- not to be confused with lineNumber!!!
    ":",
    toString currentColumnNumber(),
    webAppEndTag
    )

ZZ#{WebApp,InputPrompt} = lineno -> concatenate(
    webAppCellEndTag, -- close previous cell
    webAppCellTag,
    webAppPromptTag,
    interpreterDepth:"i",
    toString lineno,
    webAppEndTag,
    " : ",
    webAppInputTag,
    recordPosition()
)

ZZ#{WebApp,InputContinuationPrompt} = lineno -> concatenate(
    webAppInputContdTag,
    recordPosition()
    )

Thing#{WebApp,BeforePrint} = identity

Nothing#{WebApp,Print} = identity

on := () -> concatenate(webAppPromptTag,interpreterDepth:"o", toString lineNumber,webAppEndTag)

Thing#{WebApp,Print} = x -> (
    y := htmlInside x; -- we compute the html now (in case it produces an error)
    if class y =!= String then error "invalid html output";
    << endl << on() | " = " | webAppHtmlTag | y | webAppEndTag << endl;
    )

InexactNumber#{WebApp,Print} = x ->  withFullPrecision ( () -> Thing#{WebApp,Print} x )

-- afterprint


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


printFunc#WebApp = x -> (
    y := htmlInside x; -- we compute the html now (in case it produces an error)
    if class y =!= String then error "invalid html output";
    << webAppHtmlTag | y | webAppEndTag << endl;
    )

if topLevelMode === WebApp then (
    compactMatrixForm = false;
    extractStr := x -> concatenate apply(x,y -> if instance(y,Hypertext) then extractStr y else if instance(y,String) then y);
    -- the help hack: if started in WebApp mode, help is compiled in it as well
    processExamplesLoop ExampleItem := (x->new LITERAL from extractStr x) @@ (lookup(processExamplesLoop,ExampleItem));
    -- the help hack 2 (incidentally, this regex is safer than in standard mode)
    M2outputRE      = "(?="|webAppCellTag|")";
    -- the show hack
    showURL := lookup(show,URL);
    show URL := url -> if topLevelMode === WebApp then (<< webAppUrlTag | url#0 | webAppEndTag;) else showURL url;
    EDIT Sequence := x -> ((filename,start,startcol,stop,stopcol,pos,poscol) -> show URL concatenate("#editor:",filename,":",toString start,":",toString startcol,"-",toString stop,":",toString stopcol))x;
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
    html Thing := htmlLiteral1 @@ (lookup(tex,Thing)); -- ugly
)

-- the texMath hack
currentPackage#"exported mutable symbols"=append(currentPackage#"exported mutable symbols",global texMath);
currentPackage#"exported mutable symbols"=append(currentPackage#"exported mutable symbols",global html);
texMathBackup := texMath;
htmlBackup := html;
texMathInside := x -> if lookup(htmlBackup,class x) === lookup(htmlBackup,Thing) or instance(x,Expression) or instance(x,Nothing) then texMathBackup x else concatenate( -- to avoid trouble with holders
    webAppHtmlTag,
    html x,
    webAppEndTag
    );
local texMathDebug,htmlDebug;
texMathDebug = x -> concatenate(
    global texMath <- texMathBackup;
    y:=texMath class x;
    global texMath <- texMathDebug;
    "\\underset{\\tiny ",
    y,
    "}{\\fcolorbox{gray}{transparent}{$",
    if lookup(htmlBackup,class x) === lookup(htmlBackup,Thing) or instance(x,Expression) or instance(x,Nothing) then texMathBackup x else concatenate( -- to avoid trouble with holders
	webAppHtmlTag,
	htmlBackup x,
	webAppEndTag
	),
    "$}}"
    );

htmlDebug = x -> (
    flag := instance(x,Hypertext) and (try (options class x)#"xmlns" else null) =!= null; -- don't mess inside non HTML
    if flag then (
	global html <- htmlBackup;
	global texMath <- texMathInside;
	);
    y := if instance(x,Hypertext) then
    "<span class=\"M2Debug\" data-type=\"" | toString class x | "\">" | htmlBackup x | "</span>"
    else (lookup(tex,Thing)) x;
    if flag then (
	global html <- htmlDebug;
	global texMath <- texMathDebug;
    );
    y
    )
htmlInside = x -> (
    if debugLevel == 42 then (
	    global texMath <- texMathDebug;
	    global html <- htmlDebug;
	    y:=html x;
	    global texMath <- texMathBackup;
	    global html <- htmlBackup;
	    )
	else (
	    global texMath <- texMathInside;
	    y=html x;
	    global texMath <- texMathBackup;
	    );
	y);

