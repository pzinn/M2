kogan = (d,Kth,Kinv,Generic,Equivariant) -> (
    if d>9 then error "this value of d not implemented"; -- arbitrary
    koganStates1 := {" "} | apply(toList(0..d), toString); -- on slanted edges
    koganStates2 := apply(toList(0..d), toString) 
    | flatten apply(toList(0..d), j->apply(toList(0..j-1),i->toString i | toString j)); -- on horiz edges
    
    rhombi := if Equivariant then if Generic then apply(koganStates1,i->{i,i,i,i}) else {{" "," "," "," ","fill"=>"gray"}} else {}; -- actually, Generic makes no diff!!!
    if Generic then upTriangles := downTriangles := select(flatten table(koganStates1,koganStates1, (a,b) -> {a,b,if a==" " then b else if b==" " then a else concatenate sort {a,b}}), tri -> member(tri#2,koganStates2)) else (
    	basicTriangles := apply(toList(0..d),i->{" ",toString i,toString i}) 
    	| apply(toList(0..d),i->{toString i," ",toString i})
    	| flatten apply(toList(0..d), j->apply(toList(0..j-1),i->{toString i,toString j,toString i | toString j}));
	upTriangles = if Kth === false then basicTriangles else basicTriangles | toList splice table(0..Kth,Kth+1..d,(i,j)->{toString j,toString i,toString i|toString j,"fill"=>"yellow"});
	downTriangles = if Kinv === false then basicTriangles else basicTriangles | toList splice table(0..Kinv,Kinv+1..d,(i,j)->{toString j,toString i,toString i|toString j,"fill"=>"yellow"});
	-- what's going on here?? what's the value of Kth for? shouldn't it be detected automatically?
	-- TODO: have two numbers or something instead of d
	);
    (upTriangles,downTriangles,rhombi)
    )
