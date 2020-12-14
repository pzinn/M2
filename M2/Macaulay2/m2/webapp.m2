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
    y := htmlInside x; -- we compute the html now (in case it produces an error)
    if class y =!= String then error "invalid html output";
    << endl << oprompt | webAppHtmlTag | y | webAppEndTag << endl;
    )

InexactNumber#{WebApp,Print} = x ->  withFullPrecision ( () -> Thing#{WebApp,Print} x )

-- afterprint

on := () -> concatenate(interpreterDepth:"o", toString lineNumber)

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
    processExamplesLoop ExampleItem := (x->new LITERAL from replace("\\$\\{prefix\\}","usr",x#0)) @@ (lookup(processExamplesLoop,ExampleItem));
    -- the help hack 2 (incidentally, this regex is safer than in standard mode)
    M2outputRE      = "(?="|webAppCellTag|")";
    -- the print hack
    print = x -> if topLevelMode === WebApp then (
	y := htmlInside x; -- we compute the html now (in case it produces an error)
	<< webAppHtmlTag | y | webAppEndTag << endl;
	) else ( << net x << endl; );
    -- redefine htmlLiteral to exclude codes
    htmlLiteral0 := htmlLiteral;
    htmlLiteral = (s -> if s===null then null else replace(webAppTagsRegex," ",s)) @@ htmlLiteral0;
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
    flag := instance(x,Hypertext) and (options class x)#?"xmlns"; -- don't mess inside non HTML
    if flag then (
	global html <- htmlBackup;
	global texMath <- texMathInside;
	);
    y := if instance(x,Hypertext) then
    "<span class=\"M2Debug\" data-type=\"" | toString class x | "\">" | htmlBackup x | "</span>"
    else tex x;
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

