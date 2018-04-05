-- might move some texMath stuff here as well

mathJax Thing := x -> concatenate("\\(\\displaystyle ",htmlLiteral texMath x,"\\)") -- by default, for MathJax we use tex (as opposed to html)
--mathJax Thing := x -> concatenate("\\(\\require{action}\\displaystyle\\bbox[padding: 10px 0px]{\\toggle{",htmlLiteral texMath x,"}{"|htmlLiteral texMath net x|"}\\endtoggle}\\)") -- by default, for MathJax we use tex (as opposed to html)

-- text stuff
mathJax Hypertext := html -- aha!
-- see texMath Net in nets.m2
mathJax Net := n -> "<span style=\"display:inline-table;vertical-align:" | toString(5.3*(height n-1)) | "mm\">" | concatenate apply(unstack n, x-> "\\(" | texMath x | "\\)<br/>") | "</span>"
mathJax String := lookup(mathJax,Thing) -- for now. might want to switch to HTML later, just like its ancestor net
mathJax Descent := x -> "<span style=\"display:inline-table\">" | concatenate sort apply(pairs x,
     (k,v) -> (
	  if #v === 0
	  then toString k -- sucks but no choice
	  else toString k | " : " | mathJax v
	  ) | "<br/>") | "</span>"


-- output routines

ZZ#{MathJax,InputPrompt} = ZZ#{Standard,InputPrompt}
ZZ#{MathJax,InputContinuationPrompt} = ZZ#{Standard,InputContinuationPrompt}

Thing#{MathJax,BeforePrint} = identity -- not sure what to put there

texSpecial = ascii(30); -- TEMP, of course -- will turn into a valid <>

Nothing#{MathJax,Print} = identity

Thing#{MathJax,Print} = x -> (
     oprompt := concatenate(interpreterDepth:"o", toString lineNumber, " = ");
    -- compared to normal output, I don't put and endline before
    << texSpecial | "3";
    y := mathJax x; -- we compute the mathJax now (in case it produces an error)
--	<< texSpecial | "2" | oprompt | y | "<br/>" | texSpecial | "3";
	<< texSpecial | "2" | "<div style=\"padding:8px 0px\">" | oprompt | y | "</div>" | texSpecial | "3";
    )

-- afterprint <sigh>

on := () -> concatenate(interpreterDepth:"o", toString lineNumber)

texAfterPrint :=  y -> ( y = select(deepSplice sequence y, x -> class x =!= Nothing);
	 << texSpecial | "3";
	 z := htmlLiteral concatenate(texMath\y);
	 << texSpecial | "2" | on() | " : \\(" | z | "\\)<br/>" | texSpecial | "3";
	 )

Thing#{MathJax,AfterPrint} = x -> texAfterPrint class x;

Boolean#{MathJax,AfterPrint} = identity

Expression#{MathJax,AfterPrint} = x -> texAfterPrint (Expression," of class ",class x)

Describe#{MathJax,AfterPrint} = identity

Ideal#{MathJax,AfterPrint} = Ideal#{MathJax,AfterNoPrint} = (I) -> texAfterPrint (Ideal," of ",ring I)

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