--		Copyright 1993-1999 by Daniel R. Grayson

needs "expressions.m2" -- for precedence
needs "hypertext.m2"
needs "methods.m2"

peek' = method(TypicalValue => Hypertext)

peek'(ZZ,Nothing) := (depth,s) -> SPAN "null"
peek'(ZZ,Thing) := (depth,s) -> SPAN s

peek'(ZZ,VerticalList) :=
peek'(ZZ,BasicList) := (depth,s) -> SPAN (
    if depth === 0 then hold s -- hold because of Option
    else {
	class s,
	apply(toList1 s, value -> peek'(depth-1,value))
      	}
    )

peek'(ZZ,Hypertext) := (depth,s) -> SPAN (
    if depth === 0 then s
    else {
	class s,
	apply(toList1 s, value -> peek'(if instance(value,Hypertext) or instance(value,String) then depth else depth-1, value))
	}
    )

peek'(ZZ,List) := (depth,s) -> SPAN (
    if depth === 0 then { s }
    else { apply(toList1 s, value -> peek'(depth,value)) }
    )
peek'(ZZ, String) := (depth,s) -> SPAN hold ( if depth === 0 then s else format s )

formatNet := n -> (stack ((s -> substring(s,1,#s-2)) \ format \ unstack n))^(height n - 1)
peek'(ZZ,Net) := (depth,s) -> SPAN hold ( if depth === 0 then s else netList({{formatNet s}}, Boxes => true) )
peek'(ZZ,Sequence) := (depth,s) -> SPAN (
    if depth === 0 then hold s -- hold because hypertext content gets spliced when html'ed
    else if #s === 1 then {"1 : (", peek'(depth,s#0), ")"}
    else hold apply(s, value -> peek'(depth,value))
    )

peek'(ZZ,HashTable) := (depth,s) -> SPAN (
    if depth === 0 then hold s -- hold because of OptionTable
    else splice nonnull {
	class s,
	if parent s =!= Nothing then (" of ", parent s),
	VerticalList apply(
	    sortByName pairs s,
	    (key,value) -> RowExpression (
		peek'(depth-1,key),
		" ", symbol =>," ",
		peek'(depth-1,value)
		)
	    )
	}
    )

peek'(ZZ,Dictionary) := (depth,d) -> (
    if depth === 0 then SPAN d
    else SPAN {
	class d,
	VerticalList apply(sort pairs d, (lhs,rhs) -> RowExpression { peek lhs," ",symbol =>," ",peek'(depth-1,rhs) } )
	}
    )

peek = s -> peek'(1,s)
typicalValues#peek = Hypertext

ops = new MutableHashTable

seeParsing = args -> (					    -- let's eliminate this in favor of operatorAttributes
     x := new MutableHashTable;
     t := (p,s) -> (
	  if x#?p then x#p = append(x#p,s) else x#p = {s};
	  ops#s = true;
	  );
     q := getParsing symbol apply;
     scan(keys set join(values Core.Dictionary, values Core#"private dictionary"), s -> if getParsing s =!= q then t(getParsing s,s));
     t(getParsing symbol apply, "-*...symbols...*-");
     new Table from prepend(
	  { "parsing\nprecedence", "binary\nbinding\nstrength","unary\nbinding\nstrength", "\noperators" },
 	  sort pairs x / (
	       (a,b) -> append(a/(i -> if i === -1 then "" else toString i),
		    concatenate(
			 between("  ",
			      sort(b/toString)))))))

seeOperatorPrecedence = args -> (
     x := new MutableHashTable;
     t := (p,s) -> (
	  if x#?p then x#p = append(x#p,s) else x#p = {s};
	  ops#s = true;
	  );
     scan(allOperators, s -> t(first getParsing s,s));
     new Table from sort pairs x / ( (a,b) -> {toString a, concatenate( between_"  " sort(b/toString))}))

-- Local Variables:
-- compile-command: "make -C $M2BUILDDIR/Macaulay2/m2 "
-- End:
