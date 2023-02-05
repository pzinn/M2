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
    errorFlag:=false;
    topLevelMode=WebAppPrint;
    try y := html if shortMode then short x else x else errorFlag=true;  -- we compute the html now (in case it produces an error)
    topLevelMode=WebApp;
    if errorFlag or class y =!= String then error "invalid html output";
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
    -- the show, edit hacks
    showURL := lookup(show,URL);
    show URL := url -> if topLevelMode === WebApp then (<< webAppUrlTag | url#0 | webAppEndTag;) else showURL url;
    editURL := f -> URL ("/#editor:"|toString f); -- note / in URL, needed in case called outside main frame
    editMethod String :=
    EDIT FilePosition := f -> show editURL f;
    hypertext FilePosition := f -> TT HREF {editURL f,toString f};
    -- the error hack
    oldolderror := olderror;
    removeTags := s -> if s === null then null else replace(webAppTagsRegex,"😀",s);
    olderror = args -> oldolderror apply(deepSplice sequence args, removeTags);
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
-- htmlMaybe is html except if it goes thru tex
List#"delimiters" = ("{","}") -- TODO move to latex.m2 and use there
Sequence#"delimiters" = ("(",")")
Array#"delimiters" = ("[","]")

htmlMaybe = method(Dispatch=>Thing)
htmlMaybe Expression :=
htmlMaybe Nothing := x -> null
htmlMaybe Thing := x -> ( -- default test
    l := lookup(html, class x);
    if l =!= lookup(html,Thing) then l x else null
    )
-- basically, at this stage, we have to go thru every type which is silly
htmlMaybe Monoid :=
htmlMaybe RingFamily :=
htmlMaybe Ring := x -> null
htmlMaybe Descent :=
htmlMaybe Type := html
htmlMaybe HashTable := H -> if H.?texMath or not hasAttribute(H,ReverseDictionary) then null else html (TTc "constant") getAttribute(H,ReverseDictionary)
-- basic idea: anything that sits on a single line doesn't require extensible KaTeX delimiters -> just HTML it
isSimpleHypertext := c -> if c === Hypertext then true else if c === HypertextParagraph or c === HypertextContainer or c === Thing then false else isSimpleHypertext parent c
-- TODO simplify, of course using inheritance
htmlMaybe VisibleList := s -> ( -- even BasicList?
    delims := lookup("delimiters",class s); if delims === null then return;
    r := apply(s, x -> (
	    while instance(x,Holder) do x = x#0;
	    if instance(x,Symbol) or (instance(x,Number) and (class x =!= QQ or denominator x == 1)) then SPAN html x
	    else if instance(x,Ring) then (if x.?texMath or hasAttribute(x,ReverseDictionary) then SPAN html x else null)
	    else if instance(x,HashTable) and not instance(x,Type) then (if x.?texMath then SPAN html x else if hasAttribute(x,ReverseDictionary) then (TTc "constant") getAttribute(x,ReverseDictionary) else null) -- silly ... use inheritance
	    else if instance(x,VisibleList) then (
	    	r' := htmlMaybe x;
	    	if r' =!= null then LITERAL r'
	    	)
	    else hypertext x));
    if all(r, x -> isSimpleHypertext class x) then
    concatenate (
	delims#0, -- TODO KaTeX it once stabilized
	demark_", " apply(toList r,html),
	delims#1
	) else null
)
html VisibleList := s -> ( -- lame, should be automatic somehow
    h := htmlMaybe s;
    if h === null then (lookup(html,Thing)) s else h
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
