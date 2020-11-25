-- Paul Zinn-Justin 2018-2020

-- topLevelMode=WebApp definitions
-- tags are required to help the browser app distinguish html from text
webAppTags := apply((17,18,19,20,28,29,30,(18,36),(36,17)),ascii);
    (webAppEndTag,            -- closing tag ~ </span> or </p>
	webAppHtmlTag,        -- indicates what follows is HTML ~ <span class='M2Html'>
	webAppCellTag,        -- start of cell (bundled input + output) ~ <p>
	webAppInputTag,       -- it's text but it's input ~ <span class='M2Input'>
	webAppInputContdTag,  -- text, continuation of input
	webAppUrlTag,         -- used internally to follow URLs
	webAppTextTag,        -- other text ~ <span class='M2Text'>
	webAppTexTag,         -- effectively deprecated, ~ <span class='M2Html'> $
	webAppTexEndTag       -- effectively deprecated, ~ $ </span>
	)=webAppTags;

webAppTagsRegex := concatenate("[",drop(webAppTags,-2),"]")

-- now preparation for output

webAppBegin = () -> ( -- TODO remove eventually
    );
webAppEnd = () -> (
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
    webAppBegin();
    y := html x; -- we compute the html now (in case it produces an error)
    webAppEnd();
    if class y =!= String then error "invalid html output";
    << endl << oprompt | webAppHtmlTag | y | webAppEndTag << endl;
    )

InexactNumber#{WebApp,Print} = x ->  withFullPrecision ( () -> Thing#{WebApp,Print} x )

-- afterprint

on := () -> concatenate(interpreterDepth:"o", toString lineNumber)

htmlAfterPrint :=  y -> (
    y=deepSplice sequence y;
    webAppBegin();
    z := html \ y;
    webAppEnd();
    if any(z, x -> class x =!= String) then error "invalid html output";
    << endl << on() | " : " | webAppHtmlTag | concatenate z | webAppEndTag << endl;
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

Matrix#{WebApp,AfterPrint} = Matrix#{WebApp,AfterNoPrint} = f -> htmlAfterPrint (Matrix, if isFreeModule target f and isFreeModule source f then (" ", new MapExpression from {target f,source f}))

Net#{WebApp,AfterPrint} = identity

Nothing#{WebApp,AfterPrint} = identity

RingMap#{WebApp,AfterPrint} = RingMap#{WebApp,AfterNoPrint} = f -> htmlAfterPrint (class f," ",new MapExpression from {target f,source f})

Sequence#{WebApp,AfterPrint} = Sequence#{WebApp,AfterNoPrint} = identity

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
    -- the help hack: if started in WebApp mode, help is compiled in it as well
    webAppPRE := new MarkUpType of PRE; webAppPRE.qname="pre";
    html webAppPRE := x -> concatenate( -- we really mean this: the browser will interpret it as pure text so no need to htmlLiteral it
	"<pre>",
	webAppTextTag,
	apply(x,y->replace("\\$\\{prefix\\}","usr",y)), -- TEMP fix
	"\n",
	webAppEndTag,
	"</pre>\n"
	); -- TODO improve this in terms of spacing / see with css too
    pELBackup:=lookup(processExamplesLoop,ExampleItem);
    processExamplesLoop ExampleItem := x -> (
	res := pELBackup x;
	new webAppPRE from res#0 );
    -- the help hack 2 (incidentally, this regex is safer)
    M2outputRE      = "\n+(?="|webAppEndTag|webAppCellTag|")"; -- TODO: improve so cleanly separates at Cells once #1553 resolved
    -- the print hack
    print = x -> if topLevelMode === WebApp then (
	webAppBegin();
	y := html x; -- we compute the html now (in case it produces an error)
	webAppEnd();
	<< webAppHtmlTag | y | webAppEndTag << endl;
	) else ( << net x << endl; );
    -- redefine htmlLiteral to exclude codes
    htmlLiteral0 := htmlLiteral;
    htmlLiteral = (s -> if s===null then null else replace(webAppTagsRegex,"",s)) @@ htmlLiteral0;
    -- but should affect html Thing differently:
    htmlLiteral1 := s -> if s === null or regex("<|&|]]>|\42", s) === null then s else (
	ss := separate(webAppTagsRegex,s);
	depth := 0; len := 0; sss := "";
	scan(ss, x -> (
		sss = sss | (if depth == 0 then htmlLiteral0 x else x);
		len=len+#x;
		if len<#s then (
		    if s#len === webAppEndTag then depth=depth-1 else depth=depth+1;
		    sss = sss | s#len;
		    len=len+1;
		    )
		));
	sss);
    html Monoid :=
    html RingFamily :=
    html Ring :=
    html Thing := htmlLiteral1 @@ tex;
    -- the texMath hack
    currentPackage#"exported mutable symbols"=append(currentPackage#"exported mutable symbols",global texMath);
    texMathBackup := texMath;
    texMathInside := x -> if lookup(html,class x) === lookup(html,Thing) or instance(x,Expression) then texMathBackup x else concatenate( -- to avoid trouble with holders
       webAppHtmlTag,
       html x,
       webAppEndTag
       );
    webAppBegin = () -> (
	global texMath <- texMathInside;
    );
    webAppEnd = () -> (
	global texMath <- texMathBackup;
    );
)
