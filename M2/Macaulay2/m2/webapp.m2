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
    webAppTexTag         -- TeX
    ):=apply((17,18,19,20,28,30,31),ascii)


htmlWithTex Thing := tex -- by default, for KaTeX we use tex (as opposed to html)

-- text stuff: we use html instead of tex, much faster (and better spacing)
htmlWithTex Hypertext := html
-- the % is relative to line-height
htmlWithTex Net := n -> concatenate("<pre><span style=\"display:inline-table;vertical-align:",
    toString(100*(height n-1)), "%\">", apply(unstack n, x-> htmlLiteral x | "<br/>"), "</span></pre>")
htmlWithTex String := x -> concatenate("<pre>", htmlLiteral x, "</pre>") -- only problem is, this ignores starting/ending \n. but then one should use Net for that
htmlWithTex Descent := x -> concatenate("<span style=\"display:inline-table\"><pre>", sort apply(pairs x,
     (k,v) -> (
	  if #v === 0
	  then toString k -- sucks but no choice
	  else toString k | " : " | htmlWithTex v
	  ) | "<br/>"), "</pre></span>")

-- now preparation for output

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

-- experimental
texMathInsideHtml := x -> if lookup(htmlWithTex,class x) -* =!= html *- === tex then texMathBackup x else concatenate(
	webAppHtmlTag,
	htmlWithTex x,
	webAppEndTag
	);


webAppBegin := (displayStyle) -> (
    texMathStart = webAppTexTag | (if displayStyle then "\\displaystyle " else "");
    texMathEnd = webAppEndTag;
    -- the debug hack
    -*
    if expressionDebug and flag then (
	global texMath <- expressionDebugWrapper;
	global htmlWithTex <- lookup(tex,Thing); -- force the use of tex
	)
    *-
    global texMath <- texMathInsideHtml;
    )
webAppEnd := () -> (
    texMathStart = texMathEnd = "$"; -- the default tex delimiters
    -- the debug hack
    -*
    if expressionDebug then (
	global texMath <- texMathBackup;
	global htmlWithTex <- htmlWithTexBackup;
	)
    *-
    global texMath <- texMathBackup;
    )

-- output routines for WebApp mode

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

-- afterprint <sigh>

on := () -> concatenate(interpreterDepth:"o", toString lineNumber)

texAfterPrint :=  y -> (
    if instance(y,Sequence) then y=deepSplice y else y=sequence y;
    webAppBegin(false);
    z := htmlWithTex \ y;
    webAppEnd();
    << endl << on() | " : " | webAppHtmlTag | concatenate z | webAppEndTag << endl;
    )

Thing#{WebApp,AfterPrint} = x -> texAfterPrint class x;

Boolean#{WebApp,AfterPrint} = identity

Expression#{WebApp,AfterPrint} = x -> texAfterPrint (Expression," of class ",class x)

Describe#{WebApp,AfterPrint} = identity

Ideal#{WebApp,AfterPrint} = Ideal#{WebApp,AfterNoPrint} = (I) -> texAfterPrint (Ideal," of ",ring I)
MonomialIdeal#{WebApp,AfterPrint} = MonomialIdeal#{WebApp,AfterNoPrint} = (I) -> texAfterPrint (MonomialIdeal," of ",ring I)

InexactNumber#{WebApp,AfterPrint} = x -> texAfterPrint (class x," (of precision ",precision x,")")

Module#{WebApp,AfterPrint} = M -> (
     n := rank ambient M;
     texAfterPrint(ring M,"-module",
     if M.?generators then
     if M.?relations then (", subquotient of ",ambient M)
     else (", submodule of ",ambient M)
     else if M.?relations then (", quotient of ",ambient M) 
     else if n > 0 then
	  (", free",
	  if not all(degrees M, d -> all(d, zero)) 
	  then (", degrees ",runLengthEncode if degreeLength M === 1 then flatten degrees M else degrees M)
	  ))
     )


Matrix#{WebApp,AfterPrint} = Matrix#{WebApp,AfterNoPrint} = f -> texAfterPrint (Matrix, if isFreeModule target f and isFreeModule source f then (" ", new MapExpression from {target f,source f}))

Net#{WebApp,AfterPrint} = identity

Nothing#{WebApp,AfterPrint} = identity

RingMap#{WebApp,AfterPrint} = RingMap#{WebApp,AfterNoPrint} = f -> texAfterPrint (class f," ",new MapExpression from {target f,source f})

Sequence#{WebApp,AfterPrint} = Sequence#{WebApp,AfterNoPrint} = identity

CoherentSheaf#{WebApp,AfterPrint} = F -> (
     X := variety F;
     M := module F;
     n := rank ambient F;
     texAfterPrint("coherent sheaf on ",X,
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

-- bb letters (to be removed before PR)
export { "ℚ","ℝ","ℤ","ℂ","∞" }
ℚ=QQ
ℝ=RR
ℤ=ZZ
ℂ=CC
∞=infinity

if topLevelMode === WebApp then (
    -- the help hack: if started in WebApp mode, help is compiled in it as well
    webAppPRE := new MarkUpType of PRE;
    html webAppPRE := x -> concatenate( -- we really mean this: the browser will interpret it as pure text so need to htmlLiteral it
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
)

