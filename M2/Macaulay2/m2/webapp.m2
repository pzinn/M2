-- Paul Zinn-Justin 2018-2022

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
    toString currentRowNumber(), -- not to be confused with lineNumber!!!
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

protect WebAppPrint

printFunc := Thing#{WebApp,print} = x -> (
    topLevelMode=WebAppPrint;
    y := html if shortMode then short x else x; -- we compute the html now (in case it produces an error)
    topLevelMode=WebApp;
    if class y =!= String then error "invalid html output";
    << webAppHtmlTag | y | webAppEndTag << endl;
    )

on := () -> concatenate(webAppPromptTag,interpreterDepth:"o", toString lineNumber,webAppEndTag)

Thing#{WebApp,Print} = x -> (
    << endl << on() | " = ";
    printFunc x;
    )

InexactNumber#{WebApp,Print} = x ->  withFullPrecision ( () -> Thing#{WebApp,Print} x )

-- afterprint

htmlAfterPrint :=  x -> (
    << endl << on() | " : ";
    if class x === Sequence then x = RowExpression deepSplice { x };
    printFunc x;
    )

Thing#{WebApp,AfterPrint} = x -> (
    l:=lookup(AfterPrint,class x);
    if l === null then return;
    s:=l x;
    if s =!= null then htmlAfterPrint s
    )
Thing#{WebApp,AfterNoPrint} = x -> (
    l:=lookup(AfterNoPrint,class x);
    if l === null then return;
    s:=l x;
    if s =!= null then htmlAfterPrint s
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
    html FilePosition := f -> html HREF concatenate("#editor:",f#0,":",toString f#1,":",toString f#2);
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
    htmlLiteral2 := (s -> if s === null then null else replace(webAppTagsRegex," ",s)) @@ htmlLiteral0;
    -- but should affect printing differently:
    htmlLiteral1 := s -> if s === null then s else (
	depth := -1;
	concatenate apply(separate("(?="|webAppTagsRegex|")",s), x -> (
		if #x>0 and x#0 === webAppEndTag then depth=depth-1 else depth=depth+1;
		if depth <= 0 then htmlLiteral0 x else x
		)));
    htmlLiteral = s -> if topLevelMode===WebAppPrint then htmlLiteral1 s else htmlLiteral2 s;
)

-- the texMath hack
texMath1 = x -> (
    l' := lookup(html,class x);
    xx := if l' === Thing#html or instance(x,Expression) or instance(x,Nothing) then (
    	l := lookup(texMath,class x);
    	if l === null then error noMethodSingle(texMath, x, false);
    	l x
    ) else concatenate(
    webAppHtmlTag,
    l' x,
    webAppEndTag
    );
    if debugLevel != 42 then xx else concatenate(
	c:=class x;
	"\\underset{\\tiny ",
    	if c.?texMath then c.texMath else "\\texttt{"|toString c|"}",
    	"}{\\fcolorbox{gray}{transparent}{$",
    	xx,
    	"$}}"
	)
)
