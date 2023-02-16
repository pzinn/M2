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

(modes print)#WebApp = printFunc := x -> (
    y := try html if shortMode then short x else x;  -- we compute the html now (in case it produces an error)
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
    -- the show, edit hacks
    showURL := lookup(show,URL);
    show URL := url -> if topLevelMode === WebApp then (<< webAppUrlTag | url#0 | webAppEndTag;) else showURL url;
    editURL := f -> URL ("#editor:"|toString f);
    editMethod String :=
    EDIT FilePosition := f -> show editURL f;
--    fixup FilePosition := lookup(hypertext,FilePosition); -- shouldn't change that (say, in doc)
    fixup FilePosition := f -> TT HREF { f#0, toString f }; -- let's try this
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
	s=separate(delim,s);
	concatenate apply(#s, i -> if even i then removeTags htmlLiteral0 s#i else s#i)
	);
    -- misc
    html Holder := x -> if debugLevel === 42 then htmlTex x else html x#0; -- silly
    -- colored tex
    col := (c,f) -> ( x -> ///\htmlClass{token /// | c | ///}{/// | f x | ///}///);
    texMath RingFamily :=
    texMath Ring := col("class-name",lookup(texMath,HashTable));
    texMath ScriptedFunctor := col("constant",lookup(texMath,HashTable));
--    t:=col("keyword",texVariable @@ toString);
--    texMath Keyword := x -> if keywordTexMath#?x then keywordTexMath#x else t x
)

-- the html VisibleList hack
isSimpleHypertext := c -> if c === Hypertext then true else if c === HypertextParagraph or c === HypertextContainer or c === Thing then false else isSimpleHypertext parent c
-- basic idea: anything that sits on a single line doesn't require extensible KaTeX delimiters -> just HTML it
htmlInList = method(Dispatch=>Thing)
htmlInList Holder := x -> htmlInList x#0
htmlInList Type :=
htmlInList Number :=
htmlInList Symbol := html
htmlInList QQ := x -> if denominator x == 1 then html x
htmlInList ScriptedFunctor := -- or HashTable? but that creates problems with say VG types that have reverse dictionaries
htmlInList Ring := x -> if x.?texMath or hasAttribute(x,ReverseDictionary) then html x
htmlInList Thing := x -> ( h := hypertext x; if isSimpleHypertext class h then html h)  -- really, it's just TT
htmlInList VerticalList := s -> null
htmlInList Option :=
htmlInList VisibleList := s-> (
    backupFlag := htmlTexFlag; htmlTexFlag=false;
    first(html s, htmlTexFlag=backupFlag)
    )
htmlTexFlag = true -- false means only one-line is allowed
pureTexFlag = true -- means only tex, nothing else
html Option := s -> (
    if debugLevel === 42 then return htmlTex s;
    r := apply(s, x -> ( h := htmlInList x; if h =!= null then h else break));
    if r =!= null then concatenate (
	if #(r#0)<=2 or last r#0!="$" then (pureTexFlag=false; r#0|"$\\ ") else substring(r#0,0,#(r#0)-1),
	"\\ \\Rightarrow\\ ",
	if #(r#1)<=2 or first r#1!="$" then (pureTexFlag=false; "\\ $"|r#1) else substring(r#1,1)
	) else if htmlTexFlag then htmlTex s
    )
html VisibleList := s -> (
    if lookup(texMath,class s) =!= texMathVisibleList or debugLevel === 42 then return htmlTex s;
    delims := lookup("delimiters",class s);
    r := apply(s, x -> ( h := htmlInList x; if h =!= null then h else break));
    if r =!= null then 
    concatenate (
	"$",
	delims#0,
	if #s===0 then "\\," else
	demark(",\\,",apply(r,a->(
		    if #a<=2 then (pureTexFlag=false; "$"|a|"$") else (
	    		if first a==="$" then a=substring(a,1) else (pureTexFlag=false; a="$"|a);
	    		if last a==="$" then a=substring(a,0,#a-1) else (pureTexFlag=false;a=a|"$");
			a
	    		)))),
	delims#1,
	"$"
	) else if htmlTexFlag then htmlTex s
    )

-- the texMath hack
texMath1 = x -> (
    pureTexFlag=true;
    h := html x;
    if #h>2 and h#0=="$" and h#(#h-1)=="$" and pureTexFlag
    then delim|substring(h,1,#h-2)|delim
    else delim|webAppHtmlTag|h|webAppEndTag|delim -- switch back to html
)

texMath0 = x -> (
    l := lookup(texMath,class x); -- normal tex output
    if l === null then error noMethodSingle(texMath, x, false);
    xx := l x;
    if debugLevel =!= 42 then xx else concatenate(
	c:=class x;
	"\\underset{\\tiny ",
    	if c.?texMath then c.texMath else "\\texttt{"|toString c|"}",
    	"}{\\fcolorbox{gray}{transparent}{$",
    	xx,
    	"$}}"
	)
)
