-- some texMath that got stranded
texMath BasicList := s -> concatenate(
     if class s =!= List then texMath class s,
    "\\left\\{",
    between(",\\,",apply(toList s,texMath))
    ,"\\right\\}"
    )
texMath Array := x -> concatenate("\\left[", between(",", apply(x,texMath)), "\\right]")
texMath Sequence := x -> concatenate("\\left(", between(",", apply(x,texMath)), "\\right)")
texMath HashTable := x -> if x.?texMath then x.texMath else
if hasAttribute(x,ReverseDictionary) then texMath toString getAttribute(x,ReverseDictionary) else
concatenate flatten (
    texMath class x,
    "\\left\\{",
    if mutable x then if #x>0 then {"\\ldots",texMath(#x),"\\ldots"} else "" else
    between(",\\,", apply(sortByName pairs x,(k,v) -> texMath k | "\\,\\Rightarrow\\," | texMath v)),
    "\\right\\}"
    )
texMath MonoidElement := x -> texMath expression x
texMath Function := x -> texMath toString x

-- strings -- compare with hypertext.m2
texVerbLiteralTable := new MutableHashTable
    scan(characters ascii(0 .. 255), c -> texVerbLiteralTable#c = c)
    texVerbLiteralTable#"!" = ///!\texttt{!}\verb!///
    --texVerbLiteralTable#"$" = ///!\texttt{\$}\verb!/// -- eww ugly fix of #375 of mathJax. not needed if not enclosing using $
    texVerbLiteralTable#"\\"= ///!\verb!\!\verb!/// -- eww ugly fix of #375 of mathJax
    -- unfortunately the next 2 (needed if the string happens to be in a {} group) may result in wrong font in normal LaTeX depending on encoding, see https://stackoverflow.com/questions/2339651/how-to-get-real-braces-in-ttfont-in-latex
    texVerbLiteralTable#"{" =///!\texttt{\{}\verb!/// -- eww ugly fix of #375 of mathJax
    texVerbLiteralTable#"}" =///!\texttt{\}}\verb!/// -- eww ugly fix of #375 of mathJax
texVerbLiteral = s -> concatenate apply(characters s, c -> texVerbLiteralTable#c)
--texMath String := s -> "\\verb|"|texVerbLiteral s|"|"
texMath String := s -> (
    ss := separate s;
    if #ss <=1 then replace(///\\verb!!///,"",///\verb!///|texVerbLiteral s|///!///) -- to optimize compilation
    else texMath stack ss
    )

-- this truncates very big nets
maxlen := 3000; -- randomly chosen
texMath Net := n -> (
    dep := depth n; hgt := height n;
    s:="";
    len:=0; i:=0;
    scan(unstack n, x->(
	    i=i+1;
	    len=len+#x;
	    if i<#n and len>maxlen then (
		s=s|"\\vdots\\\\"|"\\vphantom{\\big|}" | texMath last n | "\\\\[-1mm]";
		if i<hgt then (hgt=i; dep=1) else dep=i+1-hgt;
		break
		);
            s=s|"\\vphantom{\\big|}" | texMath x;
            if i<#n then s=s|"\\\\[-1mm]";
	    ));
    "\\begin{array}{l}" | s | "\\end{array}"
    )

-- now the mathJax stuff per se
-- mathJax Thing produces some valid html code with possible tex code in \( \)
-- topLevelMode=MathJax produces that plus possible pure text coming from the system
-- hence, requires comments to help the browser app distinguish html from text
(mathJaxTextTag,           -- indicates what follows is pure text; default mode
    mathJaxHtmlTag,        -- indicates what follows is HTML
    mathJaxOutputTag,      -- it's html but it's output
    mathJaxInputTag,       -- it's text but it's input
    mathJaxInputContdTag):= -- text, continuation of input
apply(1..5,ascii)

oldhL := htmlLiteral;
htmlLiteral = x -> ( -- we need to protect \( and \) as well from being processed
    s := oldhL x;
    s = replace("\\\\\\(","&bsol;(",s);
    s = replace("\\\\\\)","&bsol;)",s);
    return s
    )

texWrap := x -> concatenate("\\(",htmlLiteral x,"\\)")

mathJax Thing := x -> texWrap("\\displaystyle " | texMath x) -- by default, for MathJax we use tex (as opposed to html)

-- text stuff: we use html instead of tex, much faster
mathJax Hypertext := html -- !
-- the % is relative to line-height
mathJax Net := n -> concatenate("<pre><span style=\"display:inline-table;vertical-align:", toString(100*(height n-1)), "%\">", apply(unstack n, x-> htmlLiteral x | "<br/>"), "</span></pre>")
mathJax String := x -> concatenate("<pre>", htmlLiteral x, "</pre>") -- only problem is, this ignores starting/ending \n. but then one should use Net for that
mathJax Descent := x -> concatenate("<span style=\"display:inline-table\"><pre>", sort apply(pairs x,
     (k,v) -> (
	  if #v === 0
	  then toString k -- sucks but no choice
	  else toString k | " : " | mathJax v
	  ) | "<br/>"), "</pre></span>")
-- some expressions can be mathJaxed directly w/o reference to texMath
mathJax RowExpression := x -> concatenate("<span>",apply(toList x, mathJax),"</span>")
mathJax Holder := x -> mathJax x#0
mathJax Describe := x -> mathJax x#0

-- output routines

ZZ#{MathJax,InputPrompt} = lineno -> ZZ#{Standard,InputPrompt} lineno | mathJaxInputTag
ZZ#{MathJax,InputContinuationPrompt} = lineno -> mathJaxInputContdTag

Thing#{MathJax,BeforePrint} = identity -- not sure what to put there

Nothing#{MathJax,Print} = identity

Thing#{MathJax,Print} = x -> (
    oprompt := concatenate(interpreterDepth:"o", toString lineNumber, " = ");
    mathJaxBegin();
    y := mathJax x; -- we compute the mathJax now (in case it produces an error)
    mathJaxEnd();
    << endl << oprompt | mathJaxOutputTag | y | mathJaxTextTag << endl;
    )

-- afterprint <sigh>

on := () -> concatenate(interpreterDepth:"o", toString lineNumber)

texAfterPrint :=  y -> (
    mathJaxBegin();
    z := texMath if instance(y,Sequence) then RowExpression deepSplice y else y;
    mathJaxEnd();
    << endl << on() | " : " | mathJaxHtmlTag | texWrap z | mathJaxTextTag << endl;
    )

Thing#{MathJax,AfterPrint} = x -> texAfterPrint class x;

Boolean#{MathJax,AfterPrint} = identity

Expression#{MathJax,AfterPrint} = x -> texAfterPrint (Expression," of class ",class x)

Describe#{MathJax,AfterPrint} = identity

Ideal#{MathJax,AfterPrint} = Ideal#{MathJax,AfterNoPrint} = (I) -> texAfterPrint (Ideal," of ",ring I)
MonomialIdeal#{MathJax,AfterPrint} = MonomialIdeal#{MathJax,AfterNoPrint} = (I) -> texAfterPrint (MonomialIdeal," of ",ring I)

Module#{MathJax,AfterPrint} = M -> (
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


Matrix#{MathJax,AfterPrint} = Matrix#{MathJax,AfterNoPrint} = f -> texAfterPrint (Matrix, if isFreeModule target f and isFreeModule source f then (" ", new MapArrow from {target f,source f}))

Net#{MathJax,AfterPrint} = identity

Nothing#{MathJax,AfterPrint} = identity

RingMap#{MathJax,AfterPrint} = RingMap#{MathJax,AfterNoPrint} = f -> texAfterPrint (class f," ",new MapArrow from {target f,source f})

Sequence#{MathJax,AfterPrint} = Sequence#{MathJax,AfterNoPrint} = identity

CoherentSheaf#{MathJax,AfterPrint} = F -> (
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

ZZ#{MathJax,AfterPrint} = identity

-- the debug hack
texMathDebug=false;
texMathBackup := texMath
texMathDebugWrapper := x -> (
    global texMath <- texMathBackup;
    y := texMathBackup class x;
    global texMath <- texMathDebugWrapper;
    "\\underset{\\tiny " | y | "}{\\boxed{" | texMathBackup x | "}}"
    )
mathJaxBegin = () -> (
    if texMathDebug then
    global texMath <- texMathDebugWrapper
    )
mathJaxEnd = () -> (
    if texMathDebug then
    global texMath <- texMathBackup;
    )

