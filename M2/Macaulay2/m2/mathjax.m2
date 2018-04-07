-- some texMath that got stranded
texMath BasicList := s -> concatenate(
     if class s =!= List then texMath class s,
    "\\left\\{",
    between(",\\,",apply(toList s,texMath))
    ,"\\right\\}"
    )
texMath Array := x -> concatenate("\\left[", between(",", apply(x,texMath)), "\\right]")
texMath Sequence := x -> concatenate("\\left(", between(",", apply(x,texMath)), "\\right)")
texMath HashTable := x -> if x.?texMath then x.texMath else (
     concatenate flatten (
	 texMath class x,
	 "\\left\\{",
	 between(",\\,", apply(sortByName pairs x,(k,v) -> texMath k | "\\,\\Rightarrow\\," | texMath v)),
	 "\\right\\}"
	 )
      )
texMath MonoidElement := texMath @@ expression
texMath Type := x -> if x.?texMath then x.texMath else texMath toString x
texMath Function := x -> texMath toString x
texMath ScriptedFunctor := lookup(texMath,Type)
-- for a slightly different style:
-*
texMath Type := x -> if x.?texMath then x.texMath else "{\\textsf{" | toString x | "}}"
texMath Function := x -> "{\\textsf{" | toString x | "}}"
*-

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
		s=s|"\\vdots\\\\"|"\\vphantom{\\big|}" | texMath last n | "\\\\[-1mm]"; -- "\\\\[-2mm]";
		if i<hgt then (hgt=i; dep=1) else dep=i+1-hgt;
		break
		);
	    s=s|"\\vphantom{\\big|}" | texMath x | "\\\\[-1mm]"; -- "\\\\[-2mm]";
	    ));
--    "\\raise"|toString (2.15*(-dep+hgt-1))|"mm"| -- 2.65 for [-2mm]. this number may have to be adjusted/defined more properly, disabling for now
    "\\begin{array}{l}" | s | "\\end{array}"
    )

-- now the mathJax stuff per se
-- mathJax Thing produces some valid html code with possible tex code in \( \)
-- topLevelMode=MathJax produces that plus possible pure text coming from the system
-- hence, requires comments to help the browser app distinguish html from text
mathJaxTextComment := "<!--txt-->"; -- indicates what follows is pure text; default mode
mathJaxHtmlComment := "<!--html-->"; -- indicates what follows is HTML

texWrap := x -> concatenate("\\(",htmlLiteral x,"\\)")

mathJax Thing := x -> texWrap("\\displaystyle " | texMath x) -- by default, for MathJax we use tex (as opposed to html)

-- text stuff: we use html instead of tex, much faster
mathJax Hypertext := html -- !
-- here, we assume line-height: 16px; is there a more intrinsic way to do this?
mathJax Net := n -> concatenate("<span style=\"display:inline-table;vertical-align:", toString(16*(height n-1)), "px\">", apply(unstack n, x-> mathJax x | "<br/>"), "</span>")
mathJax String := x -> concatenate("<tt>", htmlLiteral x, "</tt>")
-- a bit naive: font wrong. with mathJax can't use \tt because fix of https://github.com/mathjax/MathJax/issues/1953 is shit
-- with katex no problem just redefine <tt> to be \tt
mathJax Descent := x -> concatenate("<span style=\"display:inline-table\">", sort apply(pairs x,
     (k,v) -> (
	  if #v === 0
	  then toString k -- sucks but no choice
	  else toString k | " : " | mathJax v
	  ) | "<br/>"), "</span>")
mathJax RowExpression := x -> apply(toList x,mathJax)
-- kind of an expression analogue of Net. need to define its texMath as well
mathJax ColumnExpression := x -> concatenate("<span style=\"display:inline-table\">", apply(toList x, y->mathJax y | "<br/>"), "</span>")

-- output routines

ZZ#{MathJax,InputPrompt} = ZZ#{Standard,InputPrompt}
ZZ#{MathJax,InputContinuationPrompt} = ZZ#{Standard,InputContinuationPrompt}

Thing#{MathJax,BeforePrint} = identity -- not sure what to put there

Nothing#{MathJax,Print} = identity

Thing#{MathJax,Print} = x -> (
--    << mathJaxTextComment;
     oprompt := concatenate(interpreterDepth:"o", toString lineNumber, " = ");
    y := mathJax x; -- we compute the mathJax now (in case it produces an error)
    << endl << oprompt | mathJaxHtmlComment | y | mathJaxTextComment << endl;
    )

-- afterprint <sigh>

on := () -> concatenate(interpreterDepth:"o", toString lineNumber)

texAfterPrint :=  y -> ( y = select(deepSplice sequence y, x -> class x =!= Nothing);
--	 << mathJaxTextComment;
	 z := concatenate(texMath\y);
	 << endl << on() | " : " | mathJaxHtmlComment | texWrap z | mathJaxTextComment << endl;
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