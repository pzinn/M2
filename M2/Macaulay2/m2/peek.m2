--		Copyright 1993-1999 by Daniel R. Grayson

peek' = method(TypicalValue => Expression)

peek'(ZZ,ZZ) := (depth,n) -> Describe n
peek'(ZZ,Nothing) := (depth,s) -> Describe "null"
peek'(ZZ,Symbol) := (depth,s) -> Describe expression s
peek'(ZZ,Thing) := (depth,s) -> Describe expression s

peek'(ZZ,BasicList) := (depth,s) -> (
     if depth === 0 then Describe expression s
     else Describe Adjacent {
	  expression class s,
	  apply(toList s, value -> peek'(depth-1,value))})

peek'(ZZ,HypertextParagraph) := peek'(ZZ,Hypertext) := (depth,s) -> (
     if depth === 0 then expression s
     else Describe RowExpression {
	  expression class s,
	  new VerticalList from apply(toList s, value -> peek'(if instance(value,Hypertext) or instance(value,String) then depth else depth-1, value))
      }
  )

peek'(ZZ,List) := (depth,s) -> (
     if depth === 0 then Describe expression s
     else Describe { apply(s, value -> peek'(depth,value)) }
     )
peek'(ZZ, String) := (depth,s) -> if depth === 0 then Describe s else Describe format s

formatNet := n -> (stack ((s -> substring(s,1,#s-2)) \ format \ unstack n))^(height n - 1)
peek'(ZZ,Net) := (depth,s) -> if depth === 0 then Describe s else Describe netList({{formatNet s}}, Boxes => true)
peek'(ZZ,Sequence) := (depth,s) -> (
     if depth === 0 then Describe expression s
     else Describe { apply(s, value -> peek'(depth,value)) }
     )

precOption := precedence ( 1 => 2 )

peek'(ZZ,HashTable) := (depth,s) -> (
     if depth === 0 
     then Describe expression s
     else Describe RowExpression splice (
	  expression class s,
	  if parent s =!= Nothing 
	  then (" of ", expression parent s) else "",
	  new VerticalList from
	  apply(
	       sortByName pairs s,
	       (key,value) -> BinaryOperation ( symbol =>,
		   peek'(depth-1,key),
		   peek'(depth-1,value),
		   " "))
	  ))

peek'(ZZ,Dictionary) := (depth,d) -> (
     if depth === 0 then Describe expression d
     else Describe RowExpression splice (
	  expression class d,
	  new VerticalList from apply(sort pairs d, (lhs,rhs) -> BinaryOperation ( symbol =>, peek lhs ,peek'(depth-1,rhs)))
	  ))

peek = s -> peek'(1,s)
typicalValues#peek = Expression

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
