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

webAppPrintFlag = false

(modes print)#WebApp = printFunc := x -> (
    backupFlag := webAppPrintFlag;
    webAppPrintFlag = true;
    y := try html if shortMode then short x else x;  -- we compute the html now (in case it produces an error)
    if class y =!= String then error "invalid html output";
    << webAppHtmlTag | y | webAppEndTag << endl;
    webAppPrintFlag = backupFlag;
    )

(modes errorPrint)#WebApp = () -> (
    msg := processError errorMessage;
    s := shortMode; shortMode=false;
    print SPAN ((
	    "class"=>"M2Error",
	    if errorPosition#1>0 then SPAN{errorPosition,": ","class"=>"M2ErrorLocation"},
	    if class errorMessage =!= String or substring(errorMessage,0,2) =!= "--" then "error: "
	    ) | msg);
    shortMode=s;
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
    if class x === Sequence then x = SPAN deepSplice { x };
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

removeWebAppTags = s -> if s === null then null else replace(webAppTagsRegex,"😀",s);
if topLevelMode === WebApp then (
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
    fixup FilePosition := f -> SAMP HREF { f#0, toString f }; -- let's try this
    hypertext FilePosition := f -> SAMP HREF {editURL f,toString f};
    -- redefine htmlLiteral to exclude codes
    -- except it should sometimes allow them...
    htmlLiteral0 := htmlLiteral;
    delim:=ascii {239,187,191};
    htmlLiteral = s -> if s === null then s else (
	s=separate(delim,s);
	concatenate apply(#s, i -> if even i then removeWebAppTags htmlLiteral0 s#i else s#i)
	);
    html1 String:=htmlLiteral; -- ugh
    -- colored tex
    col := (c,f) -> ( x -> ///\htmlClass{token /// | c | ///}{/// | f x | ///}///);
    texMath RingFamily :=
    texMath Ring := col("class-name",lookup(texMath,HashTable));
    texMath HashTable := col("constant",lookup(texMath,HashTable));
    -- t:=col("keyword",texVariable @@ toString);
    -- texMath Keyword := x -> t if keywordTexMath#?x then keywordTexMath#x else x
    addEndFunction(()-> (if topLevelMode===WebApp then << webAppCellEndTag << webAppCellEndTag <<webAppCellTag << webAppCellTag;));
    )

-- show
(modes(lookup(show,URL)))#WebApp = url -> (<< webAppUrlTag | url#0 | webAppEndTag;);

-- the html hack
-- basic idea: anything that sits on a single line doesn't require extensible KaTeX delimiters -> just HTML it
multiLineFlag = true -- false means only one-line is allowed, otherwise error
pureTexFlag = true -- means only tex, nothing else
htmlTex1 = x -> (
    xx := expression x;
    if instance(xx,Holder) then (if multiLineFlag then htmlTex x else error "not one line") else html xx
    )
html QQ := x -> if multiLineFlag or denominator x == 1 then htmlTex x else error "not one line"
html Ring := x -> if x.?texMath or hasAttribute(x,ReverseDictionary) then htmlTex x else htmlTex1 x
scan({HypertextContainer,HypertextParagraph,VerticalList,Net}, t->(
	h:=lookup(html,t);
	html t := s -> if multiLineFlag then h s  else error "not one line";
	));
html2 := (fun,args,lprec,rprec,sep,supp) -> (
    if rightPrecedence fun < lprec then ( fun = sequence fun; if supp then sep="{}"; );
    if precedence args <= rprec then ( args = sequence args; if supp then sep="{}"; );
    fun = html fun;
    args = html args;
    concatenate (
	if #fun<=2 or last fun!="$" then (pureTexFlag=false; fun|"$") else substring(fun,0,#fun-1),
	sep,
	if #args<=2 or first args!="$" then (pureTexFlag=false; "$"|args) else substring(args,1)
	)
    )
html RowExpression := s -> (
    if debugLevel === 42 then return htmlTex s;
    r := apply(s, x -> try html x else break);
    if r =!= null then (
	r=new MutableList from r;
	scan(#r-1,i->(
	    	if last r#i =!= "$" or first r#(i+1) =!= "$" then pureTexFlag=false else (
		    r#i=substring(r#i,0,#(r#i)-1);
		    r#(i+1)=substring(r#(i+1),1);
		    )
	    	));
    	concatenate r   
    	)
    else if multiLineFlag then htmlTex s else error "not one line"
    )
html Expression := x -> if debugLevel === 42 or multiLineFlag then htmlTex x else error "not one line"
html Holder := x -> if debugLevel === 42 then htmlTex x else html x#0;
html Adjacent := html FunctionApplication := m -> (
    if debugLevel === 42 then return htmlTex m;
    if m#0 === sqrt or instance(m#0,Divide) then return if multiLineFlag then htmlTex m  else error "not one line";
    html2(m#0,m#1,precedence m,precedence m,
	if instance(m#1,Array) then sep:="\\mathopen{}"
    	else if instance(m#1,VisibleList) then sep="{}"
    	else sep = "\\ ",true)
    )
html BinaryOperation := b -> (
    if debugLevel === 42 then return htmlTex b;
    html2(b#1,b#2,lprec b#0,rprec b#0,if spacedOps#?(b#0) then "\\ "|htmlLiteral texMath b#0|"\\ " else htmlLiteral texMath b#0,false)
    )
html Product := html Sum := html Minus := html Constant := htmlTex
htmlList :=
html BasicList := s -> (
    if debugLevel === 42 then return htmlTex s;
    l:=lookup(texMath,class s);
    if l =!= texMathVisibleList and l =!= texMathBasicList then return htmlTex1 s;
    delims := lookup("delimiters",class s);
    backupFlag := multiLineFlag; multiLineFlag=false;
    r := apply(toList s, x -> try html x else break);
    multiLineFlag=backupFlag;
    concatenate (
	if not instance(s,VisibleList) then html class s,
	if r =!= null then (
	    "$",
	    texMath0 delims#0,
	    if #s===0 then "\\," else
	    demark(",\\,",apply(r,a->(
		    	if #a<=2 then (pureTexFlag=false; "$"|a|"$") else (
	    		    if first a==="$" then a=substring(a,1) else (pureTexFlag=false; a="$"|a);
	    		    if last a==="$" then a=substring(a,0,#a-1) else (pureTexFlag=false;a=a|"$");
			    a
	    		    )))),
	    texMath0 delims#1,
	    "$"
	    ) else if multiLineFlag then "$"|htmlLiteral texMathVisibleList s|"$" else error "not one line"
	)
    )
htmlMutable := L -> concatenate(html class L, "$\\{", if #L > 0 then "\\ldots "|#L|"\\ldots" else "\\,", "\\}$")
html MutableList  := L -> if debugLevel===42 then htmlTex L else htmlMutable L
html HashTable := H -> (
    if debugLevel === 42 or H.?texMath then htmlTex H
    else if lookup(texMath,class H) =!= lookup(texMath,HashTable) then htmlTex1 H
    else if hasAttribute(H,ReverseDictionary) then html (SAMPc "constant") getAttribute(H,ReverseDictionary)
    else if isMutable H then htmlMutable H
    else concatenate(html class H,
	htmlList apply(sortByName pairs H, p -> new Option from p)
    ))

-- the texMath hack
texMath1 = x -> if not webAppPrintFlag then texMath0 x else (
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
