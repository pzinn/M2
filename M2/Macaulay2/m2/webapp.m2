-- Paul Zinn-Justin 2018-2022

needs "expressions.m2"
needs "matrix1.m2"
needs "monideal.m2"
needs "varieties.m2"

-- topLevelMode=WebApp definitions
-- tags are required to help the browser app distinguish html from text
webAppTags := apply((17,18,19,20,28,29,30,14,21),ascii);
    (	webAppHtmlTag,        -- indicates what follows is HTML ~ <span class='M2Html'>
	webAppEndTag,         -- closing tag ~ </span>
	webAppCellTag,        -- start of cell (bundled input + output) ~ <p>
	webAppCellEndTag,     -- closing tag for cell ~ </p>
	webAppInputTag,       -- it's text but it's input ~ <span class='M2Input'>
	webAppInputContdTag,  -- text, continuation of input
	webAppUrlTag,         -- used internally to follow URLs
	webAppPromptTag,      -- input/output prompt
	webAppPositionTag     -- code position (row:col)
	)=webAppTags;

webAppTagsRegex := concatenate("[",webAppTags,"]")

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
    editMethod Sequence := args -> EDIT locate args;
    EDIT FilePosition := f -> show URL ("#editor:"|toString f);
    html FilePosition := f -> (f=toString f; html HREF ("#editor:"|f,f));
    -- the error hack
    oldolderror := olderror;
    removeTags := s -> if s === null then null else replace(webAppTagsRegex,"😀",s);
    olderror = args -> oldolderror apply(deepSplice sequence args, removeTags);
    -- the userSymbols hack (TEMP): by now mostly differs in "robust" stuff
    listSymbols List := x -> Describe TABLE prepend(
     apply({"symbol", "class", "value", "location of symbol"},s->TH {s}),
     apply(x, y -> apply({y,short class value y,short value y,locate y},s->TD {s}))
     );
    -- redefine htmlLiteral to exclude codes
    -- except it should sometimes allow them...
    htmlLiteral0 := htmlLiteral;
    delim:=ascii {239,187,191};
    htmlLiteral = s -> if s === null then s else (
	depth := 0; flag := true; -- first piece of separate is before delim, so must be ignored
	concatenate apply(separate(delim,s), x -> (
		if flag then flag=false else if #x>0 and member(first x,webAppTags) then depth=depth+1;
		first(if depth <= 0 then removeTags htmlLiteral0 x else x, if #x>0 and last x === webAppEndTag then depth=depth-1)
		)));
)

-- the texMath hack
-*
-- that stuff doesn't really work: the size of delims isn't quite right, and it still goes back and forth between tex and html too often
BareList := new Type of BasicList
html BareList := v -> demark(", ",apply(toList v,html)) -- the key point: we're redefining html BareList to force it to switch back
texMath BareList := L -> if #L > 0 then demark_",\\," apply(toList L, texMath) else "\\," -- won't ever be used directly because of prev line
texMath VisibleList := v -> "\\left\\{"|texMath (new BareList from v)|"\\right\\}" --TEMP but that's the idea
texMath Sequence := v -> "\\left("|texMath (new BareList from v)|"\\right)" --TEMP but that's the idea
*-
-- htmlMaybe is html except if it goes thru tex
htmlMaybe = method(Dispatch=>Thing)
htmlMaybe Expression :=
htmlMaybe Nothing := x -> null
htmlMaybe Thing := x -> ( -- default test
    l := lookup(html, class x);
    if l =!= texHtml then l x else null
    )
texMath1 = x -> (
    h := htmlMaybe x;
    xx := if h === null then (
    	l := lookup(texMath,class x);
    	if l === null then error noMethodSingle(texMath, x, false);
    	l x
    ) else concatenate(
    delim,
    webAppHtmlTag,
    h,
    webAppEndTag,
    delim
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
