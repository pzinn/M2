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
     if t.entry.frameID == 0
     then Code(globalAssignmentCode(t.entry,convert(p),leftPosition(t)))
     else Code(localAssignmentCode(nestingDepth(t.entry.frameID,t.dictionary),t.entry.frameindex,c:=convert(p),leftPosition(t)))
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
	       leftPosition(par.left)
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
	       leftPosition(w.forToken)
	  ))
     is w:WhileDo do Code(whileDoCode(convert(w.predicate),c:=convert(w.doClause),leftPosition(w.whileToken)))
     is w:WhileList do Code(whileListCode(convert(w.predicate),convert(w.listClause),leftPosition(w.whileToken)))
     is w:WhileListDo do Code(whileListDoCode(convert(w.predicate),convert(w.listClause),convert(w.doClause),leftPosition(w.whileToken)))
     is n:New do (
	  if n.newparent == dummyTree
	  then if n.newinitializer == dummyTree
	       then Code(newCode(c:=convert(n.newclass),leftPosition(n.newtoken)))
	       else Code(newFromCode(convert(n.newclass),convert(n.newinitializer),leftPosition(n.newtoken)))
	  else if n.newinitializer == dummyTree
	       then Code(newOfCode(convert(n.newclass),convert(n.newparent),leftPosition(n.newtoken)))
	       else Code(newOfFromCode(convert(n.newclass),convert(n.newparent),convert(n.newinitializer),leftPosition(n.newtoken))))
     is i:IfThen do Code(ifCode(convert(i.predicate),convert(i.thenClause),NullCode,leftPosition(i.ifToken)))
     is i:IfThenElse do Code(ifCode(convert(i.predicate),convert(i.thenClause),convert(i.elseClause),leftPosition(i.ifToken)))
     is token:Token do (
	  var := token.entry;
	  wrd := token.word;
	  if wrd.typecode == TCRR
	  then Code(realCode(parseRR(wrd.name),leftPosition(token)))
	  else if wrd.typecode == TCint
	  then Code(integerCode(parseInt(wrd.name),leftPosition(token)))
 	  else if wrd.typecode == TCstring
	  then (
	       s := parseString(wrd.name);
	       Code(stringCode(s,leftPosition(token)))
	       )
	  else (
	       if var.frameID == 0
	       then (
		    if var.thread
		    then Code(threadMemoryReferenceCode(var.frameindex,leftPosition(token)))
		    else Code(globalMemoryReferenceCode(var.frameindex,leftPosition(token)))
		    )
	       else Code(localMemoryReferenceCode(nestingDepth(var.frameID,token.dictionary),var.frameindex,leftPosition(token)))
	       )
	  )
     is a:Adjacent do Code(adjacentCode(convert(a.lhs),convert(a.rhs),rightPosition(a.lhs)))
     is p:EmptyParentheses do (
          pp:=leftPosition(p.left);
	  if p.left.word == leftparen then Code(sequenceCode(CodeSequence(),pp))
	  else if p.left.word == leftbrace then Code(listCode(CodeSequence(),pp))
	  else if p.left.word == leftbracket then Code(arrayCode(CodeSequence(),pp))
	  else if p.left.word == leftAngleBar then Code(angleBarListCode(CodeSequence(),pp))
	  else dummyCode			  -- should not happen
	  )
     is p:Parentheses do (
          pp:=leftPosition(p.left);
	  if p.left.word == leftparen then convert(p.contents) -- position not quite right
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
		    p := leftPosition(token);
		    if wrd.typecode == TCid
		    then (
	       		 Code(binaryCode(
			 	   b.Operator.entry.binary,
			 	   convert(b.lhs),
	       	    	 	   Code(globalSymbolClosureCode(var,p)),
			 	   leftPosition(b.Operator)
				   )
			      )
			 )
		    else dummyCode	  -- should not occur
		    )
	       else dummyCode		  -- should not occur
	       )
	  else if b.Operator.word == CommaW
	  then Code(sequenceCode(s:=makeCodeSequence(e,CommaW),leftPosition(b.Operator)))
	  else if b.Operator.word == SemicolonW
	  then Code(semiCode(s:=makeCodeSequence(e,SemicolonW),leftPosition(b.Operator)))
	  else if b.Operator.word == EqualW
	  then (
	       when b.lhs
	       is a:Adjacent do (
		    Code(
			 multaryCode(
			      InstallValueFun,
			      CodeSequence(
			      	   Code(globalSymbolClosureCode(AdjacentS.symbol,dummyPosition)),
			      	   convert(a.lhs),
			      	   convert(a.rhs),
			      	   convert(b.rhs)),
			      leftPosition(b.Operator))))
	       is u:Unary do Code(
		    ternaryCode(
			 UnaryInstallValueFun,
			 Code(globalSymbolClosureCode(u.Operator.entry,leftPosition(u.Operator))),
			 convert(u.rhs),
			 convert(b.rhs),
			 leftPosition(b.Operator)))
	       is u:Postfix do Code(
		    ternaryCode(
			 UnaryInstallValueFun,
			 Code(globalSymbolClosureCode(u.Operator.entry,leftPosition(u.Operator))),
			 convert(u.lhs),
			 convert(b.rhs),
			 leftPosition(b.Operator)))
	       is c:Binary do (
		    if c.Operator.entry == SharpS.symbol
		    then Code(ternaryCode( AssignElemFun, convert(c.lhs),
			      convert(c.rhs), convert(b.rhs), leftPosition(b.Operator)))
		    else if c.Operator.entry == DotS.symbol
		    then (
			 when c.rhs
			 is crhs:Token do
			 Code(ternaryCode(
				   AssignElemFun,
				   convert(c.lhs),
			 	   Code(globalSymbolClosureCode(crhs.entry,leftPosition(crhs))),
				   convert(b.rhs),
				   leftPosition(b.Operator)))
			 else dummyCode --should not happen
			 )
		    else Code(multaryCode(
			      InstallValueFun,
			      CodeSequence(
				   Code(globalSymbolClosureCode(c.Operator.entry,leftPosition(c.Operator))), 
				   c1:=convert(c.lhs),
				   convert(c.rhs),
				   c2:=convert(b.rhs)),
			      leftPosition(b.Operator))))
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
			      convert(b.rhs), 
			      leftPosition(b.Operator)))
		    else Code(ternaryCode(
			      AssignNewFromFun,
			      convert(n.newclass),
			      convert(n.newinitializer),
			      convert(b.rhs),
			      leftPosition(b.Operator)))
     	       	    else if n.newinitializer == dummyTree 
		    then Code(ternaryCode(
			      AssignNewOfFun,
			      convert(n.newclass),
			      convert(n.newparent),
			      convert(b.rhs),
			      leftPosition(b.Operator)))
		    else Code(multaryCode(
			      AssignNewOfFromFun,
			      CodeSequence(
				   convert(n.newclass),
				   convert(n.newparent),
				   convert(n.newinitializer),
				   convert(b.rhs)),
			      leftPosition(b.Operator))))
	       is a:Adjacent do (
		    Code(multaryCode(
			      InstallMethodFun,
			      CodeSequence(
			      	   Code(globalSymbolClosureCode(AdjacentS.symbol,dummyPosition)),
			      	   convert(a.lhs),
			      	   convert(a.rhs),
			      	   convert(b.rhs)),
		              leftPosition(b.Operator))))
	       is u:Unary do Code(ternaryCode(
			 UnaryInstallMethodFun,
			 Code(globalSymbolClosureCode(u.Operator.entry,p:=leftPosition(u.Operator))),
			 convert(u.rhs), convert(b.rhs), leftPosition(b.Operator)))
	       is u:Postfix do Code(ternaryCode(
			 UnaryInstallMethodFun,
			 Code(globalSymbolClosureCode(u.Operator.entry,leftPosition(u.Operator))),
			 convert(u.lhs), convert(b.rhs), leftPosition(b.Operator)))
	       is c:Binary do (
		    if c.Operator.entry == SharpS.symbol
		    then Code(ternaryCode( AssignElemFun, convert(c.lhs),
			      convert(c.rhs), convert(b.rhs), leftPosition(b.Operator)))
		    else if c.Operator.entry == UnderscoreS.symbol
		    then Code(multaryCode(
			      InstallMethodFun,
			      CodeSequence( 
			      	   Code(globalSymbolClosureCode(UnderscoreS.symbol,dummyPosition)),
				   convert(c.lhs),
				   convert(c.rhs),
			      	   convert(b.rhs)),
			      leftPosition(b.Operator)))
		    else if c.Operator.entry == DotS.symbol
		    then (
			 when c.rhs
			 is crhs:Token do
			 Code(ternaryCode(
				   AssignElemFun,
				   convert(c.lhs),
			 	   Code(globalSymbolClosureCode(crhs.entry,leftPosition(crhs))),
				   convert(b.rhs),
				   leftPosition(b.Operator)))
			 else dummyCode --should not happen
			 )
		    else Code(multaryCode(
			      InstallMethodFun,
			      CodeSequence(
			 	   Code(globalSymbolClosureCode(c.Operator.entry,leftPosition(c.Operator))),
				   convert(c.lhs),
				   convert(c.rhs),
				   convert(b.rhs)),
			      leftPosition(b.Operator)))
	       )
	       is t:Token do tokenAssignment(t,b.rhs)
	       is p:Parentheses do parallelAssignment(p,b.rhs,b.Operator.dictionary)
	       else dummyCode		  -- should not happen
	       )
	  else Code(binaryCode(b.Operator.entry.binary,convert(b.lhs),
	       	    convert(b.rhs),leftPosition(b.Operator)))
	  )
     is a:Arrow do (
     	       p:=leftPosition(a.lhs);
	       q:=rightPosition(a.rhs);
	       r:=leftPosition(a.Operator);
	       Code(functionCode(
	       a.Operator,		  -- just for display purposes!
	       convert(a.rhs),a.desc,nextHash(),
	       Location(p.filename, p.line,p.column, q.line,q.column, r.line,r.column, p.loadDepth)
	       )))
     is u:Unary do (
	  if u.Operator.word == CommaW
	  then Code(sequenceCode(s:=makeCodeSequence(e,CommaW),leftPosition(u.Operator)))
	  else if u.Operator.word == SemicolonW
	  then Code(semiCode(s:=makeCodeSequence(e,SemicolonW),leftPosition(u.Operator)))
	  else Code(unaryCode(u.Operator.entry.unary,convert(u.rhs),leftPosition(u.Operator))))
     is q:Quote do (
	  token := q.rhs;
	  sym := token.entry;
	  p := leftPosition(q.Operator);
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
	  p := leftPosition(q.Operator);
     	  Code(globalSymbolClosureCode(sym,p)))
     is q:ThreadQuote do (
	  token := q.rhs;
	  sym := token.entry;
	  p := leftPosition(q.Operator);
     	  Code(threadSymbolClosureCode(sym,p)))
     is q:LocalQuote do (
	  token := q.rhs;
	  sym := token.entry;
	  p := leftPosition(q.Operator);
	  nd := nestingDepth(sym.frameID,token.dictionary);
	  Code(localSymbolClosureCode(nd,sym,p)))
     is i:TryThenElse do Code(tryCode(convert(i.primary),convert(i.sequel),c:=convert(i.alternate),leftPosition(i.tryToken)))
     is i:TryElse do Code(tryCode(convert(i.primary),NullCode,c:=convert(i.alternate),leftPosition(i.tryToken)))
     is i:Try do Code(tryCode(c:=convert(i.primary),NullCode,NullCode,leftPosition(i.tryToken)))
     is i:Catch do Code(catchCode(c:=convert(i.primary),leftPosition(i.catchToken)))
     is u:Postfix do Code(unaryCode(u.Operator.entry.postfix,convert(u.lhs),leftPosition(u.Operator)))
     is d:dummy do dummyCode
     );

-- Local Variables:
-- compile-command: "echo \"make: Entering directory \\`$M2BUILDDIR/Macaulay2/d'\" && make -C $M2BUILDDIR/Macaulay2/d convertr.o "
-- End:
