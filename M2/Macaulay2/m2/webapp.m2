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

(modes errorPrint)#WebApp = (pos,msg) -> (
    pos = new FilePosition from pos;
    print PRE{SPAN {pos,"class"=>"M2ErrorLocation"},": ",if msg#0!="-" then "error: ",msg,"class"=>"M2Error"};
    if #postError>0 then {
	print DIV append(postError,"class"=>"M2Error");
	postError={};
	}
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
    -- the edit hack
    editURL := f -> URL ("#editor:"|toString f); -- TODO rewrite the edit mess using mode
    editMethod String :=
    editMethod FilePosition := f -> show editURL f;
--    fixup FilePosition := lookup(hypertext,FilePosition); -- shouldn't change that (say, in doc)
    fixup FilePosition := f -> TT HREF { f#0, toString f }; -- let's try this
    hypertext FilePosition := f -> TT HREF {editURL f,toString f};
    -- the error hack (TEMP)
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
    --
    addEndFunction(()-> (<< webAppCellEndTag << webAppCellEndTag <<webAppCellTag << webAppCellTag << endl;));
)

-- show
(modes(lookup(show,URL)))#WebApp = url -> (<< webAppUrlTag | url#0 | webAppEndTag;);

-- the html VisibleList hack
isSimpleHypertext := c -> if c === Hypertext then true else if c === HypertextParagraph or c === HypertextContainer or c === Thing then false else isSimpleHypertext parent c
-- basic idea: anything that sits on a single line doesn't require extensible KaTeX delimiters -> just HTML it
htmlInList = method(Dispatch=>Thing)
htmlInList Holder := x -> htmlInList x#0
htmlInList Type :=
htmlInList Number :=
htmlInList Symbol :=
htmlInList MutableList := html
htmlInList QQ := x -> if denominator x == 1 then html x
htmlInList Ring := x -> if x.?texMath or hasAttribute(x,ReverseDictionary) then html x
htmlInList Thing := x -> ( h := hypertext x; if isSimpleHypertext class h then html h)
htmlInList VerticalList := s -> null
htmlTexFlag = true -- false means only one-line is allowed, i.e., allows to not duplicate html -> htmlInList
pureTexFlag = true -- means only tex, nothing else
htmlInList HashTable :=
htmlInList BasicList := s-> (
    backupFlag := htmlTexFlag; htmlTexFlag=false;
    first(html s, htmlTexFlag=backupFlag)
    )
html Option := s -> ( -- might want to go thru expression? in principle could do for any BinaryOperation except not so useful
    if debugLevel === 42 then return htmlTex s;
    f := if htmlTexFlag then html else htmlInList;
    s = apply(s,expression);
    s = {if rightPrecedence s#0 < lprec symbol => then sequence s#0 else s#0,
     	if precedence s#1 <= rprec symbol => then sequence s#1 else s#1};
    r := apply(s, x -> ( h := f x; if h =!= null then h else break));
    if r =!= null then concatenate (
	if #(r#0)<=2 or last r#0!="$" then (pureTexFlag=false; r#0|"$\\ ") else substring(r#0,0,#(r#0)-1),
	"\\ \\Rightarrow\\ ",
	if #(r#1)<=2 or first r#1!="$" then (pureTexFlag=false; "\\ $"|r#1) else substring(r#1,1)
	)
    )
htmlVisibleList :=
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
html BasicList := s -> (
    if lookup(texMath,class s) =!= lookup(texMath,BasicList) or debugLevel === 42 then (if htmlTexFlag then htmlTex s)
    else concatenate(html class s,htmlVisibleList toList s)
    )
htmlMutable := L -> concatenate(html class L, "$\\{", if #L > 0 then "\\ldots "|#L|"\\ldots" else "\\,", "\\}$")
html MutableList  := L -> if debugLevel===42 then htmlTex L else htmlMutable L
-- semi-hacky: can't use hypertext cause $...$ not allowed in its output
html HashTable := H -> (
    if lookup(texMath,class H) =!= lookup(texMath,HashTable) or H.?texMath or debugLevel===42 then (if htmlTexFlag then htmlTex H)
    else if mutable H then htmlMutable H
    else (
	h:=htmlVisibleList apply(sortByName pairs H, p -> new Option from p);
	if h =!= null then concatenate(html class H,h)
    ))
html ScriptedFunctor := H -> (
    if H.?texMath or debugLevel===42 then htmlTex H
    else if hasAttribute(H,ReverseDictionary) then html (TTc "constant") getAttribute(H,ReverseDictionary)
    else htmlMutable H
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
