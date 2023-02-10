--		Copyright 1994 by Daniel R. Grayson
use binding;
use common;

dummyMultaryFun(c:CodeSequence):Expr := (
     error("dummy multary function called");
     nullE);

export AssignElemFun := dummyTernaryFun;	-- filled
export AssignQuotedElemFun := dummyTernaryFun;	-- filled
export NewFun := dummyUnaryFun;	  -- filled in later
export NewFromFun := dummyBinaryFun;	  -- filled in later
export NewOfFun := dummyBinaryFun;	  -- filled in later
export NewOfFromFun := dummyTernaryFun;	  -- filled in later

export AssignNewFun := dummyBinaryFun;
export AssignNewOfFun := dummyTernaryFun;
export AssignNewFromFun := dummyTernaryFun;
export AssignNewOfFromFun := dummyMultaryFun;

export InstallMethodFun := dummyMultaryFun;
export UnaryInstallMethodFun := dummyTernaryFun;

export InstallValueFun := dummyMultaryFun;
export UnaryInstallValueFun := dummyTernaryFun;

-- TODO sort out this mess
combineLocation(a:Location,b:Location):Location := Location(a.filename,a.line1,a.column1,b.line2,b.column2,b.line3,b.column3,a.loadDepth);
combineLocation(a:Location,b:Location,c:Location):Location := Location(a.filename,a.line1,a.column1,b.line2,b.column2,c.line3,c.column3,a.loadDepth);
combineLocation(a:Position,b:Location,c:Location):Location := Location(a.filename,a.line,a.column,b.line2,b.column2,c.line3,c.column3,a.loadDepth);
combineLocation1(a:Location,b:Location,c:Location):Location := Location(a.filename,a.line1,a.column1,b.line2,b.column2,c.line1,c.column1,a.loadDepth);
combineLocation1(a:Position,b:Location,c:Location):Location := Location(a.filename,a.line,a.column,b.line2,b.column2,c.line1,c.column1,a.loadDepth);
combineLocation2(a:Location,b:Location,c:Location):Location := Location(a.filename,a.line1,a.column1,b.line2,b.column2,c.line2,c.column2,a.loadDepth);

convert(e:ParseTree):Code;
CodeSequenceLength(e:ParseTree,separator:Word):int := (
     i := 0;
     while true do (
     	  when e
     	  is b:Binary do (
	       if b.Operator.word == separator
	       then ( i = i + CodeSequenceLength(b.rhs,separator); e = b.lhs )
	       else return i+1)
	  is u:Unary do (
	       if u.Operator.word == separator
	       then ( i = i + 1; e = u.rhs )
	       else return i+1)
	  else return i+1));
fillCodeSequence(e:ParseTree,v:CodeSequence,m:int,separator:Word):int := (
     -- Start filling v, in reverse, at position m-1, return the index of the last position filled.
     -- We do it in reverse, because our comma operator is left associative (to prevent filling the parser stack),
     -- and because we don't want to fill the stack with recursive calls to this function.
     while true do
     when e
     is b:Binary do (
	  if b.Operator.word == separator
	  then (m=fillCodeSequence(b.rhs,v,m,separator); e=b.lhs )
	  else (m=m-1; v.m=convert(e); return m))
     is u:Unary do (
	  if u.Operator.word == separator
	  then (m=fillCodeSequence(u.rhs,v,m,separator); m=m-1; v.m=Code(nullCode()); return m)
	  else (m=m-1; v.m=convert(e); return m))
     is p:EmptyParentheses do ((m=m-1; v.m=convert(e); return m))
     is dummy do (m=m-1; v.m=Code(nullCode()); return m)
     is p:Parentheses do ((m=m-1; v.m=convert(e); return m))
     else (m=m-1; v.m=convert(e); return m));
makeCodeSequence(e:ParseTree,separator:Word):CodeSequence := (
     v := new CodeSequence len CodeSequenceLength(e,separator) do provide dummyCode;
     fillCodeSequence(e,v,length(v),separator);
     v);
SymbolSequenceLength(e:ParseTree):int := (
     i := 0;
     while true do (
     	  when e
	  is p:Parentheses do e = p.contents
     	  is b:Binary do (
	       i = i+1;
	       e = b.lhs;
	       )
	  else (					    -- should be the first token
	       i = i+1;
	       return i;
	       )
	  )
     );
makeSymbolSequence(e:ParseTree):SymbolSequence := (	    -- but replace local symbols by dummySymbol
     m := SymbolSequenceLength(e);
     v := new SymbolSequence len m do provide dummySymbol;
     while true do (
     	  when e
	  is p:Parentheses do e = p.contents
     	  is b:Binary do (
	       when b.rhs is t:Token do (
		    m = m-1;
		    v.m = t.entry;
		    )
	       else nothing;				    -- shouldn't happen
	       e = b.lhs;
	       )
	  is t:Token do (
	       m = m-1;
	       v.m = t.entry;
	       break;
	       )
	  else break;					    -- shouldn't happen
	  );
     v);

nestingDepth(frameID:int,d:Dictionary):int := (
     if frameID == 0 then return -1;
     n := 0;
     while d.frameID != frameID do (
	  if !d.transient || d.framesize != 0 then n = n+1; -- empty transient frames will not appear at runtime
	  if d == d.outerDictionary then (
	       error("internal error during conversion: frameID " + tostring(frameID) + " not found");
	       break;
	       );
	  d = d.outerDictionary;
	  );
     n);

tokenAssignment(t:Token,p:ParseTree):Code := (
     c:=convert(p);
     l:=codeLocation(c);
     l=combineLocation(location(t),l,l);
     if t.entry.frameID == 0
     then Code(globalAssignmentCode(t.entry,c,l))
     else Code(localAssignmentCode(nestingDepth(t.entry.frameID,t.dictionary),t.entry.frameindex,c,l))
     );

parallelAssignment(par:Parentheses,rhs:ParseTree,d:Dictionary):Code := (
     symbols := makeSymbolSequence(ParseTree(par)); -- silly -- rethink
     n := length(symbols);
     nd := new array(int) len n do foreach x in symbols do provide nestingDepth(x.frameID,d); -- rethink dictionary
     fr := new array(int) len n do foreach x in symbols do provide x.frameindex;
     foreach x in symbols do if x.frameID != 0 then x = dummySymbol;
     Code(parallelAssignmentCode(
	       nd,
	       fr,
	       symbols,
	       c:=convert(rhs),
	       combineLocation(location(par.left),codeLocation(c),codeLocation(c)) -- TODO might need adjusting
	       ))
     );

export convert(e:ParseTree):Code := (
     when e
     is w:For do Code(
	  forCode(
	       convert(w.inClause), convert(w.fromClause), convert(w.toClause),
	       convert(w.whenClause), convert(w.listClause), 
	       c:=convert(w.doClause),
	       w.dictionary.frameID,
	       w.dictionary.framesize,
	       combineLocation(location(w.forToken),codeLocation(c),codeLocation(c)) -- TODO might need adjusting same for all 3rd args of combineLocation
	  ))
     is w:WhileDo do Code(whileDoCode(convert(w.predicate),c:=convert(w.doClause),combineLocation(location(w.whileToken),codeLocation(c),codeLocation(c))))
     is w:WhileList do Code(whileListCode(convert(w.predicate),c:=convert(w.listClause),combineLocation(location(w.whileToken),codeLocation(c),codeLocation(c))))
     is w:WhileListDo do Code(whileListDoCode(convert(w.predicate),convert(w.listClause),c:=convert(w.doClause),combineLocation(location(w.whileToken),codeLocation(c),codeLocation(c))))
     is n:New do (
	  if n.newparent == dummyTree
	  then if n.newinitializer == dummyTree
	       then Code(newCode(c:=convert(n.newclass),combineLocation(location(n.newtoken),codeLocation(c),codeLocation(c))))
	       else Code(newFromCode(convert(n.newclass),c:=convert(n.newinitializer),combineLocation(location(n.newtoken),codeLocation(c),codeLocation(c))))
	  else if n.newinitializer == dummyTree
	       then Code(newOfCode(convert(n.newclass),c:=convert(n.newparent),combineLocation(location(n.newtoken),codeLocation(c))))
	       else Code(newOfFromCode(convert(n.newclass),convert(n.newparent),c:=convert(n.newinitializer),combineLocation(location(n.newtoken),codeLocation(c)))))
     is i:IfThen do Code(ifCode(convert(i.predicate),c:=convert(i.thenClause),NullCode,combineLocation(location(i.ifToken),codeLocation(c))))
     is i:IfThenElse do Code(ifCode(convert(i.predicate),convert(i.thenClause),c:=convert(i.elseClause),combineLocation(location(i.ifToken),codeLocation(c))))
     is token:Token do (
	  var := token.entry;
	  wrd := token.word;
	  if wrd.typecode == TCRR
	  then Code(realCode(parseRR(wrd.name),location(token)))
	  else if wrd.typecode == TCint
	  then Code(integerCode(parseInt(wrd.name),location(token)))
 	  else if wrd.typecode == TCstring
	  then (
	       s := parseString(wrd.name);
	       Code(stringCode(s,location(token)))
	       )
	  else (
	       if var.frameID == 0
	       then (
		    if var.thread
		    then Code(threadMemoryReferenceCode(var.frameindex,location(token)))
		    else Code(globalMemoryReferenceCode(var.frameindex,location(token)))
		    )
	       else Code(localMemoryReferenceCode(nestingDepth(var.frameID,token.dictionary),var.frameindex,location(token)))
	       )
	  )
     is a:Adjacent do Code(adjacentCode(c:=convert(a.lhs),cc:=convert(a.rhs),combineLocation2(codeLocation(c),codeLocation(cc),codeLocation(c))))
     is p:EmptyParentheses do (
          pp:=combineLocation(location(p.left),location(p.right),location(p.left));
	  if p.left.word == leftparen then Code(sequenceCode(CodeSequence(),pp))
	  else if p.left.word == leftbrace then Code(listCode(CodeSequence(),pp))
	  else if p.left.word == leftbracket then Code(arrayCode(CodeSequence(),pp))
	  else if p.left.word == leftAngleBar then Code(angleBarListCode(CodeSequence(),pp))
	  else dummyCode			  -- should not happen
	  )
     is p:Parentheses do (
          pp:=combineLocation(location(p.left),location(p.right),location(p.left));
	  if p.left.word == leftparen then (
	     c:=convert(p.contents);
	     setLocation(c,pp); -- not quite -- fix location better
	     c
	     )
	  else if p.left.word == leftbrace 
	  then Code(listCode(makeCodeSequence(p.contents,CommaW),pp))
	  else 
	  if p.left.word == leftbracket 
	  then Code(arrayCode(makeCodeSequence(p.contents,CommaW),pp))
	  else 
	  if p.left.word == leftAngleBar
	  then Code(angleBarListCode(makeCodeSequence(p.contents,CommaW),pp))
	  else 
	  dummyCode			  -- should not happen
	  )
     is b:Binary do (
	  if b.Operator.entry == DotS.symbol
	  || b.Operator.entry == DotQuestionS.symbol
	  then (
	       when b.rhs
	       is token:Token do (
	  	    wrd := token.word;
		    var := token.entry;
		    p := location(token);
		    if wrd.typecode == TCid
		    then (
	       		 Code(binaryCode(
			 	   b.Operator.entry.binary,
			 	   c:=convert(b.lhs),
	       	    	 	   Code(globalSymbolClosureCode(var,p)),
			 	   combineLocation(codeLocation(c),p,location(b.Operator))
				   )
			      )
			 )
		    else dummyCode	  -- should not occur
		    )
	       else dummyCode		  -- should not occur
	       )
	  else if b.Operator.word == CommaW
	  then Code(sequenceCode(s:=makeCodeSequence(e,CommaW),combineLocation(codeLocation(s.0),codeLocation(s.(length(s)-1)),codeLocation(s.0))))
	  else if b.Operator.word == SemicolonW
	  then Code(semiCode(s:=makeCodeSequence(e,SemicolonW),combineLocation(codeLocation(s.0),codeLocation(s.(length(s)-1)),codeLocation(s.0))))
	  else if b.Operator.word == EqualW
	  then (
	       when b.lhs
	       is a:Adjacent do (
		    Code(
			 multaryCode(
			      InstallValueFun,
			      CodeSequence(
			      	   Code(globalSymbolClosureCode(AdjacentS.symbol,dummyLocation)),
			      	   c:=convert(a.lhs),
			      	   convert(a.rhs),
			      	   cc:=convert(b.rhs)),
			      combineLocation(codeLocation(c),codeLocation(cc),location(b.Operator)))))
	       is u:Unary do Code(
		    ternaryCode(
			 UnaryInstallValueFun,
			 Code(globalSymbolClosureCode(u.Operator.entry,p:=location(u.Operator))),
			 convert(u.rhs),
			 cc:=convert(b.rhs),
			 combineLocation(p,codeLocation(cc),location(b.Operator))))
	       is u:Postfix do Code(
		    ternaryCode(
			 UnaryInstallValueFun,
			 Code(globalSymbolClosureCode(u.Operator.entry,location(u.Operator))),
			 c:=convert(u.lhs),
			 cc:=convert(b.rhs),
			 combineLocation(codeLocation(c),codeLocation(cc),location(b.Operator))))
	       is c:Binary do (
		    if c.Operator.entry == SharpS.symbol
		    then Code(ternaryCode( AssignElemFun, c1:=convert(c.lhs),
			      convert(c.rhs), c2:=convert(b.rhs), combineLocation(codeLocation(c1),codeLocation(c2),location(b.Operator))))
		    else if c.Operator.entry == DotS.symbol
		    then (
			 when c.rhs
			 is crhs:Token do
			 Code(ternaryCode(
				   AssignElemFun,
				   c1:=convert(c.lhs),
			 	   Code(globalSymbolClosureCode(crhs.entry,location(crhs))),
				   c2:=convert(b.rhs),
				   combineLocation(codeLocation(c1),codeLocation(c2),location(b.Operator))))
			 else dummyCode --should not happen
			 )
		    else Code(multaryCode(
			      InstallValueFun,
			      CodeSequence(
				   Code(globalSymbolClosureCode(c.Operator.entry,location(c.Operator))), 
				   c1:=convert(c.lhs),
				   convert(c.rhs),
				   c2:=convert(b.rhs)),
			      combineLocation(codeLocation(c1),codeLocation(c2),location(b.Operator)))))
	       is t:Token do tokenAssignment(t,b.rhs)
	       is p:Parentheses do parallelAssignment(p,b.rhs,b.Operator.dictionary)
	       else dummyCode		  -- should not happen
	       )
	  else if b.Operator.word == ColonEqualW
	  then (
	       when b.lhs
	       is n:New do (
		    if n.newparent == dummyTree 
		    then if n.newinitializer == dummyTree 
		    then Code(binaryCode(
			      AssignNewFun,
			      convert(n.newclass),
			      c:=convert(b.rhs), 
			      combineLocation(location(n.newtoken),codeLocation(c),location(b.Operator))))
		    else Code(ternaryCode(
			      AssignNewFromFun,
			      convert(n.newclass),
			      convert(n.newinitializer),
			      c:=convert(b.rhs),
			      combineLocation(location(n.newtoken),codeLocation(c),location(b.Operator))))
     	       	    else if n.newinitializer == dummyTree 
		    then Code(ternaryCode(
			      AssignNewOfFun,
			      convert(n.newclass),
			      convert(n.newparent),
			      c:=convert(b.rhs),
			      combineLocation(location(n.newtoken),codeLocation(c),location(b.Operator))))
		    else Code(multaryCode(
			      AssignNewOfFromFun,
			      CodeSequence(
				   convert(n.newclass),
				   convert(n.newparent),
				   convert(n.newinitializer),
				   c:=convert(b.rhs)),
			      combineLocation(location(n.newtoken),codeLocation(c),location(b.Operator)))))
	       is a:Adjacent do (
		    Code(multaryCode(
			      InstallMethodFun,
			      CodeSequence(
			      	   Code(globalSymbolClosureCode(AdjacentS.symbol,dummyLocation)),
			      	   c:=convert(a.lhs),
			      	   convert(a.rhs),
			      	   cc:=convert(b.rhs)),
			      combineLocation(codeLocation(c),codeLocation(cc),location(b.Operator)))))
	       is u:Unary do Code(ternaryCode(
			 UnaryInstallMethodFun,
			 Code(globalSymbolClosureCode(u.Operator.entry,p:=location(u.Operator))),
			 convert(u.rhs), c:=convert(b.rhs), combineLocation(p,codeLocation(c),location(b.Operator))))
	       is u:Postfix do Code(ternaryCode(
			 UnaryInstallMethodFun,
			 Code(globalSymbolClosureCode(u.Operator.entry,location(u.Operator))),
			 c:=convert(u.lhs), cc:=convert(b.rhs), combineLocation(codeLocation(c),codeLocation(cc),location(b.Operator))))
	       is c:Binary do (
		    if c.Operator.entry == SharpS.symbol
		    then Code(ternaryCode( AssignElemFun, c1:=convert(c.lhs),
			      convert(c.rhs), c2:=convert(b.rhs), combineLocation(codeLocation(c1),codeLocation(c2),location(b.Operator))))
		    else if c.Operator.entry == UnderscoreS.symbol
		    then Code(multaryCode(
			      InstallMethodFun,
			      CodeSequence( 
			      	   Code(globalSymbolClosureCode(UnderscoreS.symbol,dummyLocation)),
				   c1:=convert(c.lhs),
				   convert(c.rhs),
			      	   c2:=convert(b.rhs)),
			      combineLocation(codeLocation(c1),codeLocation(c2),location(b.Operator))))
		    else if c.Operator.entry == DotS.symbol
		    then (
			 when c.rhs
			 is crhs:Token do
			 Code(ternaryCode(
				   AssignElemFun,
				   c1:=convert(c.lhs),
			 	   Code(globalSymbolClosureCode(crhs.entry,location(crhs))),
				   c2:=convert(b.rhs),
				   combineLocation(codeLocation(c1),codeLocation(c2),location(b.Operator))))
			 else dummyCode --should not happen
			 )
		    else Code(multaryCode(
			      InstallMethodFun,
			      CodeSequence(
			 	   Code(globalSymbolClosureCode(c.Operator.entry,location(c.Operator))),
				   c1:=convert(c.lhs),
				   convert(c.rhs),
				   c2:=convert(b.rhs)),
			      combineLocation(codeLocation(c1),codeLocation(c2),location(b.Operator))))
	       )
	       is t:Token do tokenAssignment(t,b.rhs)
	       is p:Parentheses do parallelAssignment(p,b.rhs,b.Operator.dictionary)
	       else dummyCode		  -- should not happen
	       )
	  else Code(binaryCode(b.Operator.entry.binary,c1:=convert(b.lhs),
	       	    c2:=convert(b.rhs),combineLocation(codeLocation(c1),codeLocation(c2),location(b.Operator))))
	  )
     is a:Arrow do (
     	       p:=leftPosition(a.lhs); -- ~ treePosition
	       Code(functionCode(
	       a.Operator,		  -- just for display purposes!
	       c:=convert(a.rhs),a.desc,nextHash(),
	       combineLocation1(p,codeLocation(c),codeLocation(c))
	       )))
     is u:Unary do (
	  if u.Operator.word == CommaW
	  then Code(sequenceCode(s:=makeCodeSequence(e,CommaW),combineLocation(location(u.Operator),codeLocation(s.(length(s)-1)))))
	  else if u.Operator.word == SemicolonW
	  then Code(semiCode(s:=makeCodeSequence(e,SemicolonW),combineLocation(location(u.Operator),codeLocation(s.(length(s)-1)))))
	  else Code(unaryCode(u.Operator.entry.unary,c:=convert(u.rhs),combineLocation(location(u.Operator),codeLocation(c),location(u.Operator)))))
     is q:Quote do (
	  token := q.rhs;
	  sym := token.entry;
	  p := combineLocation(location(q.Operator),location(token)); -- TODO check
	  if sym.frameID == 0
	  then (
	       if sym.thread
	       then Code(threadSymbolClosureCode(sym,p))
	       else Code(globalSymbolClosureCode(sym,p))
	       )
	  else Code(localSymbolClosureCode(nestingDepth(sym.frameID,token.dictionary),sym,p)))
     is q:GlobalQuote do (
	  token := q.rhs;
	  sym := token.entry;
	  p := combineLocation(location(q.Operator),location(token)); -- TODO check
     	  Code(globalSymbolClosureCode(sym,p)))
     is q:ThreadQuote do (
	  token := q.rhs;
	  sym := token.entry;
	  p := combineLocation(location(q.Operator),location(token)); -- TODO check
     	  Code(threadSymbolClosureCode(sym,p)))
     is q:LocalQuote do (
	  token := q.rhs;
	  sym := token.entry;
	  p := combineLocation(location(q.Operator),location(token)); -- TODO check
	  nd := nestingDepth(sym.frameID,token.dictionary);
	  Code(localSymbolClosureCode(nd,sym,p)))
     is i:TryThenElse do Code(tryCode(convert(i.primary),convert(i.sequel),c:=convert(i.alternate),combineLocation(location(i.tryToken),codeLocation(c))))
     is i:TryElse do Code(tryCode(convert(i.primary),NullCode,c:=convert(i.alternate),combineLocation(location(i.tryToken),codeLocation(c))))
     is i:Try do Code(tryCode(c:=convert(i.primary),NullCode,NullCode,combineLocation(location(i.tryToken),codeLocation(c))))
     is i:Catch do Code(catchCode(c:=convert(i.primary),combineLocation(location(i.catchToken),codeLocation(c))))
     is u:Postfix do Code(unaryCode(u.Operator.entry.postfix,c:=convert(u.lhs),combineLocation(codeLocation(c),location(u.Operator))))
     is d:dummy do dummyCode
     );

-- Local Variables:
-- compile-command: "echo \"make: Entering directory \\`$M2BUILDDIR/Macaulay2/d'\" && make -C $M2BUILDDIR/Macaulay2/d convertr.o "
-- End:
