-- Paul Zinn-Justin 2018

-- htmlWithTex Thing produces some valid html code with possible TeX code
-- topLevelMode=WebApp produces that plus possible pure text coming from the system
-- hence, requires tags to help the browser app distinguish html from text
webAppTags := apply((17,18,19,20,28,29,30,31,17),ascii);
    (webAppEndTag,            -- closing tag ~ </span>
	webAppHtmlTag,        -- indicates what follows is HTML ~ <span class='M2Html'>
	webAppCellTag,        -- start of cell (bundled input + output) ~ <p>
	webAppInputTag,       -- it's text but it's input ~ <span class='M2Input'>
	webAppInputContdTag,  -- text, continuation of input
	webAppUrlTag,         -- used internally to follow URLs
	webAppTextTag,        -- other text ~ <span class='M2Text'>
	webAppTexTag,         -- TeX start ~ \(
	webAppTexEndTag       -- TeX end ~ \)
	)=webAppTags;

texMathStart := "\\(";
texMathEnd := "\\)";
texWrap := x -> (  -- similar to 'tex', but only used by webapp.m2 to avoid thread-safety issues -- TODO: rewrite in a thread-safe way
    y := texMath x;
    if class y =!= String then error "invalid texMath output";
    texMathStart | y | texMathEnd
    )

htmlWithTex Thing := texWrap -- by default, we use tex (as opposed to html)

webAppTagsRegex := concatenate("[",webAppTags,"]")
stripTags := s -> replace(webAppTagsRegex,"",s)

-- text stuff: we use html instead of tex, much faster (and better spacing)
htmlWithTex Hypertext := html
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

-- now preparation for output

webAppBegin = (displayStyle) -> (
    texMathStart = webAppTexTag | (if displayStyle then "\\displaystyle " else "");
    texMathEnd = webAppTexEndTag;
    );
webAppEnd = () -> (
    texMathStart = "\\(";
    texMathEnd = "\\)";
    );

-- output routines for WebApp mode

ZZ#{WebApp,InputPrompt} = lineno -> concatenate(
    webAppEndTag, -- close previous cell
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
    webAppBegin(true);
    y := htmlWithTex x; -- we compute the htmlWithTex now (in case it produces an error)
    webAppEnd();
    if class y =!= String then error "invalid htmlWithTex output";
    << endl << oprompt | webAppHtmlTag | y | webAppEndTag << endl;
    )

InexactNumber#{WebApp,Print} = x ->  withFullPrecision ( () -> Thing#{WebApp,Print} x )

-- afterprint

on := () -> concatenate(interpreterDepth:"o", toString lineNumber)

htmlWithTexAfterPrint :=  y -> (
    y=deepSplice sequence y;
    webAppBegin(false);
    z := htmlWithTex \ y;
    webAppEnd();
    if any(z, x -> class x =!= String) then error "invalid htmlWithTex output";
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

if topLevelMode === WebApp then (
    -- the help hack: if started in WebApp mode, help is compiled in it as well
    webAppPRE := new MarkUpType of PRE; webAppPRE.qname="pre";
    html webAppPRE := x -> concatenate( -- we really mean this: the browser will interpret it as pure text so no need to htmlLiteral it
	"<pre>",
	webAppTextTag,
	replace("\\$\\{prefix\\}","usr",x#0), -- TEMP fix
	"\n",
	webAppEndTag,
	"</pre>\n"
	);
    pELBackup:=lookup(processExamplesLoop,ExampleItem);
    processExamplesLoop ExampleItem := x -> (
	res := pELBackup x;
	new webAppPRE from res#0 );
    -- the help hack 2
    M2outputRE      = "(\n+)"|webAppEndTag|webAppCellTag;
   -- the print hack
    print = x -> if topLevelMode === WebApp then (
	webAppBegin(true);
	y := htmlWithTex x; -- we compute the htmlWithTex now (in case it produces an error)
	webAppEnd();
	<< webAppHtmlTag | y | webAppEndTag << endl;
	) else ( << net x << endl; );
    -- the texMath hack
    currentPackage#"exported mutable symbols"=append(currentPackage#"exported mutable symbols",global texMath);
    currentPackage#"exported mutable symbols"=append(currentPackage#"exported mutable symbols",global html);
    texMathBackup := texMath;
    htmlBackup := html;
    texMathInside := x -> if lookup(htmlWithTex,class x) === texWrap then texMathBackup x else concatenate(
	webAppHtmlTag,
	htmlWithTex x,
	webAppEndTag
	);
    -- ideally we'd just have htmlInside = htmlWithTex but not that simple... (String, Net...)
    htmlInside := x -> if lookup(htmlWithTex,class x) === texWrap then texWrap x else htmlBackup x;
    webAppBegin = (displayStyle) -> (
	texMathStart = webAppTexTag | (if displayStyle then "\\displaystyle " else "");
	texMathEnd = webAppTexEndTag;
	global texMath <- texMathInside;
	global html <- htmlInside;
    );
    webAppEnd = () -> (
	texMathStart = "\\(";
	texMathEnd = "\\)";
	global texMath <- texMathBackup;
	global html <- htmlBackup;
    );
)
